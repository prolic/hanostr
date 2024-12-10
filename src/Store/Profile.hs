module Store.Profile where

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

import Nostr.Keys (PubKeyXO)
import Nostr.Types (Profile, emptyProfile)
import Store.LMDB (defaultJsonSettings)
import Types (AppState(..))

data ProfileStore :: Effect where
    PutProfile :: PubKeyXO -> (Profile, Int) -> ProfileStore m ()
    GetProfile :: PubKeyXO -> ProfileStore m (Profile, Int)
    DeleteProfile :: PubKeyXO -> ProfileStore m ()

type instance DispatchOf ProfileStore = Dynamic

makeEffect ''ProfileStore

runProfileStore :: (IOE :> es, State AppState :> es)
                => Eff (ProfileStore : es) a 
                -> Eff es a
runProfileStore = interpret $ \_ -> \case
    PutProfile pk newProf@(profile, newTimestamp) -> do
        st <- get
        case (lmdbEnv st, profileDb st) of
            (Just env, Just db) -> liftIO $ withTransaction env $ \txn -> do
                let readTxn = readonly txn
                existing <- Map.lookup' readTxn db pk
                let finalProf = case existing of
                        Just (existingProfile, existingTimestamp) -> 
                            if existingTimestamp >= newTimestamp 
                                then (existingProfile, existingTimestamp)
                                else newProf
                        Nothing -> newProf
                Map.insert' txn db pk finalProf
            _ -> throwIO $ userError "Profile database not initialized"

    GetProfile pk -> do
        st <- get
        case (lmdbEnv st, profileDb st) of
            (Just env, Just db) -> liftIO $ withTransaction env $ \txn -> do
                let readTxn = readonly txn
                mProfile <- Map.lookup' readTxn db pk
                case mProfile of
                    Just (profile, timestamp) -> return (profile, timestamp)
                    Nothing -> return (emptyProfile, 0)
            _ -> throwIO $ userError "Profile database not initialized"

    DeleteProfile pk -> do
        st <- get
        case (lmdbEnv st, profileDb st) of
            (Just env, Just db) -> liftIO $ withTransaction env $ \txn ->
                Map.delete' txn db pk
            _ -> throwIO $ userError "Profile database not initialized"

initProfileDb :: Transaction 'ReadWrite -> IO (Database PubKeyXO (Profile, Int))
initProfileDb txn = openDatabase txn (Just "profiles") defaultJsonSettings
