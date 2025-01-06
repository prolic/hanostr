{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Store.Lmdb
    ( LmdbStore(..)
    , LmdbState(..)
    , TimelineType(..)
    , initialLmdbState
    , initializeLmdbState
    , runLmdbStore
    , putEvent
    , putGiftWrap
    , getEvent
    , getFollows
    , getProfile
    , getTimelineIds
    ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (forM_,void)
import Data.Aeson (ToJSON, FromJSON, encode, decode, eitherDecode)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Effectful
import Effectful.Exception (throwIO)
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared (State, get)
import Effectful.State.Static.Shared qualified as State
import Effectful.FileSystem
import Effectful.TH (makeEffect)
import Lmdb.Codec qualified as Codec
import Lmdb.Connection
import Lmdb.Map qualified as Map
import Lmdb.Types
import Pipes.Prelude qualified as Pipes
import System.FilePath ((</>))
import Pipes ((>->))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

import Logging
import Nostr.Event (validateEvent, unwrapGiftWrap, unwrapSeal)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Types ( Event(..), EventId(..), Kind(..), Profile, Relay(..), Tag(..)
                   , Rumor(..), rumorPubKey, rumorTags, rumorCreatedAt, emptyProfile )
import Nostr.Util
import Types (AppState(..), EventWithRelays(..), Follow(..))


-- | Timeline types
data TimelineType = PostTimeline | ChatTimeline
    deriving (Show, Eq)

-- | Timeline key type
type TimelineKey = (PubKeyXO, Int)

-- | LMDB state containing all database handles
data LmdbState = LmdbState
    { lmdbLock :: MVar ()
    , lmdbEnv :: Environment ReadWrite
    , eventDb :: Database EventId EventWithRelays
    , profileDb :: Database PubKeyXO (Profile, Int)
    , postTimelineDb :: Database (PubKeyXO, Int) EventId
    , chatTimelineDb :: Database (PubKeyXO, Int) EventId
    , followsDb :: Database PubKeyXO [Follow]
    } deriving (Generic)


-- | LmdbStore operations
data LmdbStore :: Effect where
    -- Event operations (now handles all storage operations)
    PutEvent :: EventWithRelays -> LmdbStore m ()
    PutGiftWrap :: EventWithRelays -> [PubKeyXO] -> Int -> LmdbStore m ()
    
    -- Query operations (read-only)
    GetEvent :: EventId -> LmdbStore m (Maybe EventWithRelays)
    GetFollows :: PubKeyXO -> LmdbStore m [Follow]
    GetProfile :: PubKeyXO -> LmdbStore m (Profile, Int)
    GetTimelineIds :: TimelineType -> PubKeyXO -> Int -> LmdbStore m [EventId]


type instance DispatchOf LmdbStore = Dynamic

makeEffect ''LmdbStore


-- | Run LmdbEffect
runLmdbStore :: (Util :> es,IOE :> es, State LmdbState :> es, Logging :> es)
             => Eff (LmdbStore : es) a
             -> Eff es a
runLmdbStore = interpret $ \env -> \case
    -- Event operations (main storage operation)
    PutEvent ev -> do
        LmdbState{..} <- get
        kp <- getKeyPair
        liftIO $ withMVar lmdbLock $ \_ -> withTransaction lmdbEnv $ \txn -> do
            Map.repsert' txn eventDb (eventId $ event ev) ev

            case kind (event ev) of
                GiftWrap -> do
                    mSealedEvent <- liftIO $ unwrapGiftWrap (event ev) kp
                    case mSealedEvent of
                        Just sealedEvent | validateEvent sealedEvent -> 
                            case kind sealedEvent of
                                Seal -> do
                                    mDecryptedRumor <- liftIO $ unwrapSeal sealedEvent kp
                                    case mDecryptedRumor of
                                        Just decryptedRumor | pubKey sealedEvent == rumorPubKey decryptedRumor -> do
                                            let participants = if rumorPubKey decryptedRumor == keyPairToPubKeyXO kp
                                                  then sort $ getAllPTags (rumorTags decryptedRumor)
                                                  else filter (/= keyPairToPubKeyXO kp) $ rumorPubKey decryptedRumor : sort (getAllPTags (rumorTags decryptedRumor))
                                            addTimelineEntryTx txn chatTimelineDb ev participants (rumorCreatedAt decryptedRumor)
                                        _ -> pure ()
                                _ -> pure ()
                        _ -> pure ()

                ShortTextNote -> 
                    addTimelineEntryTx txn postTimelineDb ev [pubKey $ event ev] (createdAt $ event ev)

                Repost -> do
                    let etags = [t | t@(ETag _ _ _) <- tags (event ev)]
                    let mOriginalEvent = eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev)
                    case (etags, mOriginalEvent) of
                        (ETag _ _ _:_, Right originalEvent) | validateEvent originalEvent -> 
                            addTimelineEntryTx txn postTimelineDb ev [pubKey $ event ev] (createdAt $ event ev)
                        _ -> pure ()

                EventDeletion -> do
                    let eventIdsToDelete = [eid | ETag eid _ _ <- tags (event ev)]
                    forM_ eventIdsToDelete $ \eid -> do
                        mEvent <- Map.lookup' (readonly txn) eventDb eid
                        case mEvent of
                            Just deletedEv -> do
                                let key = (pubKey $ event deletedEv, createdAt $ event deletedEv)
                                    db = case kind (event deletedEv) of
                                        ShortTextNote -> postTimelineDb
                                        Repost -> postTimelineDb
                                        _ -> chatTimelineDb
                                Map.delete' txn db key
                                Map.delete' txn eventDb eid
                            Nothing -> pure ()

                Metadata -> 
                    case eitherDecode (fromStrict $ encodeUtf8 $ content $ event ev) of
                        Right profile -> 
                            Map.repsert' txn profileDb (pubKey $ event ev) (profile, createdAt $ event ev)
                        Left _ -> pure ()

                FollowList -> do
                    let followList' = [Follow pk (fmap InboxRelay relay') petName' | PTag pk relay' petName' <- tags (event ev)]
                        authorPk = pubKey $ event ev
                    mExisting <- Map.lookup' (readonly txn) followsDb authorPk
                    Map.repsert' txn followsDb authorPk followList'

                _ -> pure ()

    GetEvent eid -> do
        LmdbState{..} <- get
        liftIO $ withTransaction lmdbEnv $ \txn ->
            Map.lookup' (readonly txn) eventDb eid

    -- Query operations (read-only)
    GetFollows pk -> do
        LmdbState{..} <- get
        liftIO $ withTransaction lmdbEnv $ \txn -> do
            mFollows <- Map.lookup' (readonly txn) followsDb pk
            pure $ maybe [] id mFollows

    GetProfile pk -> do
        LmdbState{..} <- get
        liftIO $ withTransaction lmdbEnv $ \txn -> do
            mProfile <- Map.lookup' (readonly txn) profileDb pk
            pure $ maybe (emptyProfile, 0) id mProfile

    GetTimelineIds timelineType author limit -> do
        LmdbState{..} <- get
        let timelineDb = case timelineType of
                PostTimeline -> postTimelineDb
                ChatTimeline -> chatTimelineDb
        liftIO $ withTransaction lmdbEnv $ \txn ->
            withCursor (readonly txn) timelineDb $ \cursor ->
                Pipes.toListM $
                    Map.lastBackward cursor
                    >-> Pipes.filter (\kv -> fst (keyValueKey kv) == author)
                    >-> Pipes.map keyValueValue
                    >-> Pipes.take limit

-- Helper function for timeline entries within a transaction
addTimelineEntryTx :: Transaction 'ReadWrite 
                   -> Database TimelineKey EventId
                   -> EventWithRelays 
                   -> [PubKeyXO] 
                   -> Int 
                   -> IO ()
addTimelineEntryTx txn timelineDb' ev pks timestamp = do
    let invertedTimestamp = maxBound - timestamp
    withCursor txn timelineDb' $ \cursor ->
        forM_ pks $ \pk ->
            Map.repsert cursor (pk, invertedTimestamp) (eventId $ event ev)


-- | Default Lmdb settings for JSON-serializable types with better error handling
defaultJsonSettings :: (Ord k, ToJSON k, FromJSON k, ToJSON v, FromJSON v, Show k, Show v) 
                   => DatabaseSettings k v
defaultJsonSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (toStrict . encode)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right x -> Just x
            Left err -> trace ("Failed to decode key: " ++ err) Nothing))
    (Codec.throughByteString
        (toStrict . encode)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right x -> Just x
            Left err -> trace ("Failed to decode value: " ++ err) Nothing))


-- Lmdb configuration
maxMapSize :: Int
maxMapSize = 500_000_000_000 -- 500 GB

maxReaders :: Int
maxReaders = 120

maxDbs :: Int
maxDbs = 8 -- currently 5 are required, leave some room for future growth

-- | Initialize the Lmdb environment
initializeEnv :: FilePath -> IO (Environment ReadWrite, MVar ())
initializeEnv dbPath = do
    env <- initializeReadWriteEnvironment maxMapSize maxReaders maxDbs dbPath
    lock <- newMVar ()
    pure (env, lock)

-- | Initialize the event database
initEventDb :: Transaction 'ReadWrite -> IO (Database EventId EventWithRelays)
initEventDb txn = openDatabase txn (Just "events") eventDbSettings

-- | Initialize the follows database
initFollowsDb :: Transaction 'ReadWrite -> IO (Database PubKeyXO [Follow])
initFollowsDb txn = openDatabase txn (Just "follows") followsDbSettings

-- | Initialize the profile database
initProfileDb :: Transaction 'ReadWrite -> IO (Database PubKeyXO (Profile, Int))
initProfileDb txn = openDatabase txn (Just "profiles") defaultJsonSettings

-- | Initialize the post timeline database
initPostTimelineDb :: Transaction 'ReadWrite -> IO (Database TimelineKey EventId)
initPostTimelineDb txn = openDatabase txn (Just "post_timeline") defaultJsonSettings

-- | Initialize the chat timeline database
initChatTimelineDb :: Transaction 'ReadWrite -> IO (Database TimelineKey EventId)
initChatTimelineDb txn = openDatabase txn (Just "chat_timeline") defaultJsonSettings

-- | Settings for the event database
eventDbSettings :: DatabaseSettings EventId EventWithRelays
eventDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(EventId bs) -> bs)
        (Just . EventId))
    (Codec.throughByteString
        (\(EventWithRelays ev rs) -> toStrict $ encode (ev, Set.toList rs))
        (\bs -> case decode (fromStrict bs) of
            Just (ev, rsList) -> Just $ EventWithRelays ev (Set.fromList rsList)
            Nothing -> Nothing))


-- | Settings for the follows database
followsDbSettings :: DatabaseSettings PubKeyXO [Follow]
followsDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\pk -> toStrict $ encode pk)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right pk -> Just pk
            Left _ -> Nothing))
    (Codec.throughByteString
        (\follows -> toStrict $ encode follows)
        (\bs -> case eitherDecode (fromStrict bs) of
            Right follows -> Just follows
            Left _ -> Nothing))


-- | Get all p tags from the rumor tags
getAllPTags :: [Tag] -> [PubKeyXO]
getAllPTags = mapMaybe extractPubKey
  where
    extractPubKey (PTag pk _ _) = Just pk
    extractPubKey _ = Nothing

-- | Initialize LMDB state
initializeLmdbState :: FilePath -> IO LmdbState
initializeLmdbState dbPath = do
    (env, lock) <- initializeEnv dbPath
    withTransaction env $ \txn -> do
        eventDb' <- initEventDb txn
        followsDb' <- initFollowsDb txn
        profileDb' <- initProfileDb txn
        postTimelineDb' <- initPostTimelineDb txn
        chatTimelineDb' <- initChatTimelineDb txn
        pure $ LmdbState
            { lmdbLock = lock
            , lmdbEnv = env
            , eventDb = eventDb'
            , profileDb = profileDb'
            , postTimelineDb = postTimelineDb'
            , chatTimelineDb = chatTimelineDb'
            , followsDb = followsDb'
            }

-- | Initial LMDB state before login
initialLmdbState :: LmdbState
initialLmdbState = LmdbState
    { lmdbLock = error "LMDB not initialized"
    , lmdbEnv = error "LMDB not initialized"
    , eventDb = error "LMDB not initialized"
    , profileDb = error "LMDB not initialized"
    , postTimelineDb = error "LMDB not initialized"
    , chatTimelineDb = error "LMDB not initialized"
    , followsDb = error "LMDB not initialized"
    }
