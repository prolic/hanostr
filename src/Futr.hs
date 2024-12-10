{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}

module Futr where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Aeson (ToJSON, pairs, toEncoding, (.=))
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Proxy (Proxy(..))
import Data.Set qualified as Set
import Data.Text (Text, isPrefixOf, pack, unpack)
import Data.Typeable (Typeable)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically, readTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception (SomeException, try)
import Effectful.FileSystem
  ( FileSystem,
    XdgDirectory (XdgData),
    createDirectoryIfMissing,
    getXdgDirectory
  )
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
import Lmdb.Connection (closeEnvironment, withCursor, withTransaction)
import Lmdb.Map qualified as LMap
import Lmdb.Types (KeyValue(..))
import QtQuick
import GHC.Generics (Generic)
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Graphics.QML qualified as QML
import Pipes.Prelude qualified as Pipes
import System.FilePath ((</>))

import Logging
import KeyMgmt (Account(..), AccountId(..), KeyMgmt, KeyMgmtState(..))
import Nostr
import Nostr.Bech32
import Nostr.Event ( createComment, createEventDeletion, createFollowList
                   , createQuoteRepost, createRepost, createRumor, createShortTextNote
                   )
import Nostr.Keys (PubKeyXO, derivePublicKeyXO, keyPairToPubKeyXO, secKeyToKeyPair)
import Nostr.GiftWrap
import Nostr.Publisher
import Nostr.RelayConnection (RelayConnection)
import Nostr.RelayPool
import Nostr.Subscription
import Nostr.Types ( Event(..), EventId, Profile(..), Relay(..), RelayURI, Tag(..)
                   , getUri, metadataFilter )
import Nostr.Util
import Presentation.KeyMgmtUI (KeyMgmtUI)
import Presentation.RelayMgmtUI (RelayMgmtUI)
import RelayMgmt (RelayMgmt)
import Store.LMDB (initializeEnv)
import Store.Event (EventStore, getEvent, initEventDb)
import Store.Profile (ProfileStore, getProfile, initProfileDb)
import Types hiding (Comment, QuoteRepost, Repost)

-- | Signal key class for LoginStatusChanged.
data LoginStatusChanged deriving Typeable

instance SignalKeyClass LoginStatusChanged where
    type SignalParams LoginStatusChanged = Bool -> Text -> IO ()


-- | Search result.
data SearchResult
  = ProfileResult { npub :: Text, relayUri :: Maybe Text }
  | NoResult
  deriving (Eq, Generic, Show)

instance ToJSON SearchResult where
  toEncoding (ProfileResult npub' relayUri') = pairs
     ( "npub"  .= npub'
    <> "relayUri" .= relayUri'
     )
  toEncoding NoResult = pairs ( "result" .= ("no_result" :: Text) )


-- | Futr Effects.
data Futr :: Effect where
  Login :: ObjRef () -> Text -> Futr m ()
  Search :: ObjRef () -> Text -> Futr m SearchResult
  SetCurrentProfile :: Text -> Futr m ()
  FollowProfile :: Text -> Futr m ()
  UnfollowProfile :: Text -> Futr m ()
  OpenChat :: PubKeyXO -> Futr m ()
  SendPrivateMessage :: Text -> Futr m ()
  SendShortTextNote :: Text -> Futr m ()
  Logout :: ObjRef () -> Futr m ()
  Repost :: EventId -> Futr m ()
  QuoteRepost :: EventId -> Text -> Futr m ()
  Comment :: EventId -> Text -> Futr m ()
  DeleteEvent :: EventId -> Text -> Futr m ()


-- | Dispatch type for Futr effect.
type instance DispatchOf Futr = Dynamic


makeEffect ''Futr


-- | Effectful type for Futr.
type FutrEff es = ( State AppState :> es
                  , EventStore :> es
                  , ProfileStore :> es
                  , KeyMgmt :> es
                  , KeyMgmtUI :> es
                  , RelayMgmtUI :> es
                  , Nostr :> es
                  , RelayConnection :> es
                  , RelayMgmt :> es
                  , RelayPool :> es
                  , Subscription :> es
                  , Publisher :> es
                  , State KeyMgmtState :> es
                  , State RelayPoolState :> es
                  , State QtQuickState :> es
                  , GiftWrap :> es
                  , QtQuick :> es
                  , Logging :> es
                  , IOE :> es
                  , FileSystem :> es
                  , Concurrent :> es
                  , Util :> es
                  )


-- | Run the Futr effect.
runFutr :: FutrEff es => Eff (Futr : es) a -> Eff es a
runFutr = interpret $ \_ -> \case
  Login obj input -> do
      kst <- get @KeyMgmtState
      case Map.lookup (AccountId input) (accountMap kst) of
        Just a -> do
          logInfo $ "Starting login for account: " <> pack (show $ accountPubKeyXO a)
          let pk = accountPubKeyXO a

          dbResult <- try @SomeException $ do
              baseDir <- getXdgDirectory XdgData ("futrnostr" </> unpack (pubKeyXOToBech32 pk))
              let lmdbDir = baseDir </> "db"
              createDirectoryIfMissing True lmdbDir
              env <- liftIO $ initializeEnv lmdbDir
              (eventsDb, profilesDb) <- liftIO $ withTransaction env $ \txn -> do
                  events <- initEventDb txn
                  profiles <- initProfileDb txn
                  pure (events, profiles)
              modify @AppState $ \st -> st {
                  lmdbEnv = Just env,
                  eventDb = Just eventsDb,
                  profileDb = Just profilesDb
              }

              -- Load existing events from database asynchronously
              withEffToIO (ConcUnlift Persistent Unlimited) $ \runE ->
                liftIO $ withTransaction env $ \txn -> do
                  withCursor txn eventsDb $ \cur -> do
                    events <- Pipes.toListM (LMap.firstForward cur)
                    runE $ do
                        logInfo $ "Reading " <> pack (show (length events)) <> " events from local storage"
                        void $ async $ do
                          forM_ events $ \kv -> do
                            let firstRelay = maybe "local" id $ Set.lookupMin $ relays $ keyValueValue kv
                            handleEvent' (event $ keyValueValue kv) firstRelay True

          case dbResult of
            Left e -> do
              logError $ "Database initialization failed: " <> pack (show e)
              liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Database initialization failed"
              return ()
            Right _ -> do
              loginWithAccount obj a

  Search _ input -> do
    st <- get @AppState
    let myPubKey = keyPairToPubKeyXO <$> keyPair st

    if not ("nprofile" `isPrefixOf` input || "npub" `isPrefixOf` input)
      then return NoResult
      else case parseNprofileOrNpub input of
        Nothing -> return NoResult
        Just (pubkey', maybeRelay) 
          | Just pubkey' == myPubKey -> return NoResult
          | otherwise -> do
              let userFollows = maybe [] (flip (Map.findWithDefault []) (follows st)) myPubKey
              if any (\(Follow pk _ _) -> pk == pubkey') userFollows
                then return $ ProfileResult (pubKeyXOToBech32 pubkey') maybeRelay
                else do
                  searchInRelays pubkey' maybeRelay
                  return $ ProfileResult (pubKeyXOToBech32 pubkey') maybeRelay

  SetCurrentProfile npub' -> do
    case bech32ToPubKeyXO npub' of
      Just pk -> do
        modify @AppState $ \st -> st { currentProfile = Just pk }
        notify $ emptyUpdates { profilesChanged = True }
      Nothing -> do
        logError $ "Invalid npub, cannot set current profile: " <> npub'
        return ()

  FollowProfile npub' -> do
    let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    case keyPairToPubKeyXO <$> keyPair st of
        Just userPK -> do
            let currentFollows = Map.findWithDefault [] userPK (follows st)
            unless (any (\follow -> pubkey follow == pubKeyXO) currentFollows) $ do
                let newFollow = Follow pubKeyXO Nothing Nothing
                let newFollows = newFollow : currentFollows
                modify $ \st' -> st' { follows = Map.insert userPK newFollows (follows st') }
                notify $ emptyUpdates { followsChanged = True }
            sendFollowListEvent
        Nothing -> return ()

  UnfollowProfile npub' -> do
    let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npub'
    st <- get @AppState
    let userPubKey = keyPairToPubKeyXO <$> keyPair st
    case userPubKey of
        Just userPK -> do
            let currentFollows = Map.findWithDefault [] userPK (follows st)
            let newFollows = filter (\follow -> pubkey follow /= pubKeyXO) currentFollows
            modify $ \st' -> st' { follows = Map.insert userPK newFollows (follows st') }
            notify $ emptyUpdates { followsChanged = True }
            sendFollowListEvent
        Nothing -> return ()

  OpenChat pubKeyXO -> do
    st <- get @AppState

    case currentContact st of
      (Just _, Just subId') -> stopSubscription subId'
      _ -> return ()

    modify $ \st' -> st' { currentContact = (Just pubKeyXO, Nothing) }
    notify $ emptyUpdates { privateMessagesChanged = True }

  SendPrivateMessage input -> do
    st <- get @AppState
    case (keyPair st, currentContact st) of
      (Just kp, (Just recipient, _)) -> do
        now <- getCurrentTime
        let senderPubKeyXO = keyPairToPubKeyXO kp
            allRecipients = nub [senderPubKeyXO, recipient]
            rumor = createRumor senderPubKeyXO now (map (\xo -> PTag xo Nothing Nothing) [recipient]) input

        giftWraps <- forM allRecipients $ \recipient' -> do
          seal <- createSeal rumor kp recipient'
          case seal of
            Just seal' -> do
              giftWrapResult <- createGiftWrap seal' recipient'
              case giftWrapResult of
                Just (gw, _) -> return (Just gw)
                Nothing -> logError "Failed to create gift wrap" >> return Nothing
            Nothing -> logError "Failed to create seal" >> return Nothing

        let validGiftWraps = catMaybes giftWraps
        forM_ validGiftWraps $ \gw -> publishGiftWrap gw senderPubKeyXO

      (Nothing, _) -> logError "No key pair found"
      (_, (Nothing, _)) -> logError "No current chat recipient"

  SendShortTextNote input -> do
    kp <- getKeyPair
    now <- getCurrentTime
    let u = createShortTextNote input (keyPairToPubKeyXO kp) now
    signed <- signEvent u kp
    case signed of
      Just s -> publishToOutbox s
      Nothing -> logError "Failed to sign short text note"

  Logout obj -> do
      -- Close LMDB environment if it exists
      st <- get @AppState
      case lmdbEnv st of
        Just env -> liftIO $ closeEnvironment env
        Nothing -> return ()

      -- Reset application state
      modify @AppState $ \st -> st
        { keyPair = Nothing
        , currentScreen = KeyMgmt
        , lmdbEnv = Nothing
        , eventDb = Nothing
        , profileDb = Nothing
        , follows = Map.empty
        }

      -- Close relay connections
      conns <- gets @RelayPoolState activeConnections
      mapM_ disconnect (Map.keys conns)

      -- Wait a moment for disconnects to process
      threadDelay 100000  -- 100ms delay

      modify @RelayPoolState $ const initialRelayPoolState

      fireSignal obj
      logInfo "User logged out successfully"

  Repost eid -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- getEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just EventWithRelays{event, relays} -> do
            let e = createRepost event (Set.findMin relays) (keyPairToPubKeyXO kp) now
            signed <- signEvent e kp
            case signed of
              Just s -> do
                publishToOutbox s
                mEventAndRelays <- getEvent eid
                case mEventAndRelays of
                  Just EventWithRelays{event = origEvent, relays = relaySet} -> do
                    let eventRelayUris = Set.fromList $ map getUri $
                          catMaybes [Just r | RelayTag r <- tags origEvent]

                    authorRelays <- gets @RelayPoolState $ \st' ->
                      maybe [] (filter isInboxCapable . fst) $
                      Map.lookup (pubKey origEvent) (generalRelays st')

                    let authorInboxUris = Set.fromList $ map getUri authorRelays
                        targetUris = eventRelayUris `Set.union` authorInboxUris `Set.union` relaySet

                    forM_ (Set.toList targetUris) $ \relay ->
                      publishToRelay s relay
                  Nothing -> return ()
              Nothing -> logError "Failed to sign repost"

  QuoteRepost eid quote -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- getEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just EventWithRelays{relays} | Set.null relays -> do
            logError "Failed to fetch event: no relays"
          Just EventWithRelays{event, relays} -> do
            let q = createQuoteRepost event (Set.findMin relays) quote (keyPairToPubKeyXO kp) now
            signed <- signEvent q kp
            case signed of
              Just s -> publishToOutbox s
              Nothing -> logError "Failed to sign quote repost"

  Comment eid comment' -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        mEvent <- getEvent eid
        case mEvent of
          Nothing -> logError $ "Failed to fetch event " <> pack (show eid)
          Just EventWithRelays{event, relays} -> do
            let c = createComment event comment' (Right eid) Nothing Nothing (keyPairToPubKeyXO kp) now
            signed <- signEvent c kp
            case signed of
              Just s -> do
                publishToOutbox s
                mEventAndRelays <- getEvent eid
                case mEventAndRelays of
                  Just EventWithRelays{event = origEvent, relays = relaySet} -> do
                    let eventRelayUris = Set.fromList $ map getUri $
                          catMaybes [Just r | RelayTag r <- tags origEvent]

                    authorRelays <- gets @RelayPoolState $ \st' ->
                      maybe [] (filter isInboxCapable . fst) $
                      Map.lookup (pubKey origEvent) (generalRelays st')

                    let authorInboxUris = Set.fromList $ map getUri authorRelays
                        targetUris = eventRelayUris `Set.union` authorInboxUris `Set.union` relaySet

                    forM_ (Set.toList targetUris) $ \relay ->
                      publishToRelay s relay
                  Nothing -> return ()
              Nothing -> logError "Failed to sign comment"

  DeleteEvent eid reason -> do
    st <- get @AppState
    case keyPair st of
      Nothing -> logError "No keypair found"
      Just kp -> do
        now <- getCurrentTime
        let deletion = createEventDeletion [eid] reason (keyPairToPubKeyXO kp) now
        signed <- signEvent deletion kp
        case signed of
          Just s -> publishToOutbox s
          Nothing -> logError "Failed to sign event deletion"


-- Helper function to parse nprofile or npub
parseNprofileOrNpub :: Text -> Maybe (PubKeyXO, Maybe RelayURI)
parseNprofileOrNpub input = 
  case bech32ToPubKeyXO input of
    Just pubkey' -> Just (pubkey', Nothing)  -- for npub
    Nothing -> case nprofileToPubKeyXO input of
      Just (pubkey', relays') -> Just (pubkey', listToMaybe relays')  -- for nprofile
      Nothing -> Nothing


-- | Login with an account.
loginWithAccount :: FutrEff es => ObjRef () -> Account -> Eff es ()
loginWithAccount obj a = do
    let (rs, t) = accountRelays a

    modify @AppState $ \s -> s { keyPair = Just (secKeyToKeyPair $ accountSecKey a) }

    modify @KeyMgmtState $ \st -> st
        { nsecView = secKeyToBech32 $ accountSecKey a
        , npubView = pubKeyXOToBech32 $ derivePublicKeyXO $ accountSecKey a
        }

    importGeneralRelays (accountPubKeyXO a) rs t

    forM_ rs $ \relay' -> void $ async $ connect $ getUri relay'

    void $ async $ do
        atLeastOneConnected <- awaitAtLeastOneConnected
        -- Update UI state after connections are established
        when atLeastOneConnected $ do
            modify @AppState $ \s -> s { currentScreen = Home }
            fireSignal obj

        -- Fire final status
        if not atLeastOneConnected 
            then liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj False "Failed to connect to any relay"
            else liftIO $ QML.fireSignal (Proxy :: Proxy LoginStatusChanged) obj True ""


-- | Send a follow list event.
sendFollowListEvent :: FutrEff es => Eff es ()
sendFollowListEvent = do
  st <- get @AppState
  case keyPair st of
    Nothing -> logError "No keypair found"
    Just kp -> do
      currentTime <- getCurrentTime
      let userPK = keyPairToPubKeyXO kp
          followList' = Map.findWithDefault [] userPK (follows st)
          followTuples = map (\(Follow pk _ petName') -> (pk, petName')) followList'
          event = createFollowList followTuples userPK currentTime
      signedEvent <- signEvent event kp
      case signedEvent of
        Just signedEvent' -> publishToOutbox signedEvent'
        Nothing -> do
          logError "Failed to sign follow list event"
          return ()


-- | Search for a profile in relays.
searchInRelays :: FutrEff es => PubKeyXO -> Maybe RelayURI -> Eff es ()
searchInRelays pubkey' _ = do
    -- @todo use relay hint
    st <- get @RelayPoolState
    let relays = case Map.lookup pubkey' (generalRelays st) of
                   Just (rs, _) -> rs
                   Nothing -> []
    conns <- gets @RelayPoolState activeConnections
    forM_ relays $ \relay -> do
      when (isInboxCapable relay) $ do
        let relayUri' = getUri relay
        when (Map.member relayUri' conns) $ do
          subId' <- newSubscriptionId
          mq <- subscribe relayUri' subId' [metadataFilter [pubkey']]
          case mq of
              Nothing -> return ()
              Just q -> void $ async $ do
                  let loop = do
                        e <- atomically $ readTQueue q
                        case e of
                          EventAppeared event' -> do
                            updates <- handleEvent relayUri' subId' [metadataFilter [pubkey']] event'
                            notify updates
                            loop
                          SubscriptionEose -> do
                            stopSubscription subId'
                            loop
                          SubscriptionClosed _ -> return () -- stop the loop
                  loop


isInboxCapable :: Relay -> Bool
isInboxCapable (InboxRelay _) = True
isInboxCapable (InboxOutboxRelay _) = True
isInboxCapable _ = False


-- | Get the content of a post.
getPostContent :: FutrEff es => Post -> Eff es (Maybe Text)
getPostContent post = do
    mEvent <- getEvent (postId post)
    return $ fmap (content . event) mEvent


-- | Get the creation timestamp of a post.
getPostCreatedAt :: FutrEff es => Post -> Eff es (Maybe Int)
getPostCreatedAt post = do
    mEvent <- getEvent (postId post)
    return $ fmap (createdAt . event) mEvent


-- | Get the content of a referenced post.
getReferencedContent :: FutrEff es => EventId -> Eff es (Maybe Text)
getReferencedContent eid = do
    mEvent <- getEvent eid
    return $ fmap (content . event) mEvent


-- | Get the author of a referenced post.
getReferencedAuthor :: FutrEff es => EventId -> Eff es (Maybe PubKeyXO)
getReferencedAuthor eid = do
    mEvent <- getEvent eid
    return $ fmap (pubKey . event) mEvent


-- | Get the author name of a referenced post.
getReferencedAuthorName :: FutrEff es => EventId -> Eff es (Maybe Text)
getReferencedAuthorName eid = do
    mEvent <- getEvent eid
    case mEvent of
        Nothing -> return Nothing
        Just EventWithRelays{event} -> do
            let authorPubKey = pubKey event
            (profile, _) <- getProfile authorPubKey
            return $ Just $ fromMaybe
                (fromMaybe (pubKeyXOToBech32 authorPubKey) (name profile))
                (displayName profile)


-- | Get the author picture of a referenced post.
getReferencedAuthorPicture :: FutrEff es => EventId -> Eff es (Maybe Text)
getReferencedAuthorPicture eid = do
    mEvent <- getEvent eid
    case mEvent of
        Nothing -> return Nothing
        Just EventWithRelays{event} -> do
            (profile, _) <- getProfile (pubKey event)
            return $ picture profile


-- | Get the creation timestamp of a referenced post.
getReferencedCreatedAt :: FutrEff es => EventId -> Eff es (Maybe Int)
getReferencedCreatedAt eid = do
    mEvent <- getEvent eid
    return $ fmap (createdAt . event) mEvent
