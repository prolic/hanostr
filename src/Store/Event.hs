module Store.Event where

import Data.ByteString.Lazy qualified as LBS
import Data.Aeson (encode, decode)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Exception
import Effectful.FileSystem
  ( FileSystem,
    XdgDirectory (XdgData),
    createDirectoryIfMissing,
    getXdgDirectory
  )
import Effectful.State.Static.Shared (State, get)
import Effectful.TH (makeEffect)
import Lmdb.Types
import Lmdb.Connection
import Lmdb.Map qualified as Map
import Lmdb.Codec qualified as Codec
import System.FilePath ((</>))
import qualified Control.Exception

import Nostr.Types (EventId(..))
import Store.LMDB (defaultJsonSettings)
import Types (AppState(..), EventWithRelays(..))


data EventStore :: Effect where
    PutEvent :: EventId -> EventWithRelays -> EventStore m ()
    GetEvent :: EventId -> EventStore m (Maybe EventWithRelays)
    DeleteEvent :: EventId -> EventStore m ()

type instance DispatchOf EventStore = Dynamic

makeEffect ''EventStore


-- | Run EventStore operations with LMDB
runEventStore :: (IOE :> es, State AppState :> es)
              => Eff (EventStore : es) a 
              -> Eff es a
runEventStore = interpret $ \_ -> \case
    PutEvent eid ev -> do
        st <- get
        case (lmdbEnv st, eventDb st) of
            (Just env, Just db) -> liftIO $ withTransaction env $ \txn -> do
                let readTxn = readonly txn
                existing <- Map.lookup' readTxn db eid
                let mergedEvent = case existing of
                        Just existingEv -> 
                            ev { relays = Set.union (relays ev) (relays existingEv) }
                        Nothing -> ev
                Map.insert' txn db eid mergedEvent
            _ -> throwIO $ userError "Event database not initialized"

    GetEvent eid -> do
        st <- get
        case (lmdbEnv st, eventDb st) of
            (Just env, Just db) -> liftIO $ withTransaction env $ \txn -> do
                let readTxn = readonly txn
                Map.lookup' readTxn db eid
            _ -> throwIO $ userError "Event database not initialized"

    DeleteEvent eid -> do
        st <- get
        case (lmdbEnv st, eventDb st) of
            (Just env, Just db) -> liftIO $ withTransaction env $ \txn -> 
                Map.delete' txn db eid
            _ -> throwIO $ userError "Event database not initialized"


-- | Settings for the event database
eventDbSettings :: DatabaseSettings EventId EventWithRelays
eventDbSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (\(EventId bs) -> bs)
        (Just . EventId))
    (Codec.throughByteString
        (\(EventWithRelays ev rs) -> LBS.toStrict $ encode (ev, Set.toList rs))
        (\bs -> case decode (LBS.fromStrict bs) of
            Just (ev, rsList) -> Just $ EventWithRelays ev (Set.fromList rsList)
            Nothing -> error "Failed to decode EventWithRelays"))


-- | Initialize the event database
initEventDb :: Transaction 'ReadWrite -> IO (Database EventId EventWithRelays)
initEventDb txn = openDatabase txn (Just "events") eventDbSettings
