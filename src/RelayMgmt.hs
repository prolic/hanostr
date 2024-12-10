{-# LANGUAGE BlockArguments #-}

module RelayMgmt where

import Control.Monad (forM)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH

import QtQuick
import KeyMgmt (AccountId(..), KeyMgmt, updateRelays)
import Logging
import Nostr
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Event (createPreferredDMRelaysEvent, createRelayListMetadataEvent)
import Nostr.Keys (PubKeyXO)
import Nostr.Publisher
import Nostr.RelayConnection
import Nostr.Types (Relay(..), RelayURI, getUri)
import Nostr.Util
import Types (ConnectionState(..), RelayPoolState(..), RelayData(..))


-- | Effect for handling RelayMgmt operations.
data RelayMgmt :: Effect where
    -- General Relay Management
    ImportGeneralRelays :: PubKeyXO -> [Relay] -> Int -> RelayMgmt m ()
    AddGeneralRelay :: PubKeyXO -> RelayURI -> Bool -> Bool -> RelayMgmt m Bool
    RemoveGeneralRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()
    GetGeneralRelays :: PubKeyXO -> RelayMgmt m ([(Relay, ConnectionState)], Int)
    -- DM Relay Management
    ImportDMRelays :: PubKeyXO -> [Relay] -> Int -> RelayMgmt m ()
    AddDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m Bool
    RemoveDMRelay :: PubKeyXO -> RelayURI -> RelayMgmt m ()
    GetDMRelays :: PubKeyXO -> RelayMgmt m ([(Relay, ConnectionState)], Int)

type instance DispatchOf RelayMgmt = Dynamic

makeEffect ''RelayMgmt


-- | RelayMgmtEff
type RelayMgmtEff es =
  ( State RelayPoolState :> es
  , Nostr :> es
  , RelayConnection :> es
  , Publisher :> es
  , KeyMgmt :> es
  , Logging :> es
  , QtQuick :> es
  , Util :> es
  )


-- | Handler for relay pool effects.
runRelayMgmt
  :: RelayMgmtEff es
  => Eff (RelayMgmt : es) a
  -> Eff es a
runRelayMgmt = interpret $ \_ -> \case
    ImportGeneralRelays pk rs ts -> do
        let rs' = map normalizeRelay rs
        modify @RelayPoolState $ \st -> do
            case Map.lookup pk (generalRelays st) of
                Nothing -> st { generalRelays = Map.insert pk (rs', ts) (generalRelays st) }
                Just (_, existingTs) -> 
                    if ts > existingTs
                        then st { generalRelays = Map.insert pk (rs', ts) (generalRelays st) }
                        else st
        notifyRelayStatus

    AddGeneralRelay pk relay' r w -> do
        if not r && not w
            then
                return False
            else do
                let relay'' = case (r, w) of
                        (True, True) -> InboxOutboxRelay $ normalizeRelayURI relay'
                        (True, False) -> InboxRelay $ normalizeRelayURI relay'
                        (False, True) -> OutboxRelay $ normalizeRelayURI relay'
                        (False, False) -> error "Unreachable due to guard above"
                kp <- getKeyPair
                st' <- get @RelayPoolState
                let (existingRelays, _) = Map.findWithDefault ([], 0) pk (generalRelays st')
                if relay'' `elem` existingRelays
                    then return False
                    else do
                        let rs = relay'' : existingRelays
                        now <- getCurrentTime
                        modify @RelayPoolState $ \st'' -> st''
                            { generalRelays = Map.insert pk (rs, now) (generalRelays st'') }
                        updateRelays (AccountId $ pubKeyXOToBech32 pk) (rs, now)
                        notifyRelayStatus
                        let unsigned = createRelayListMetadataEvent rs pk now
                        signed <- signEvent unsigned kp
                        case signed of
                            Just signed' -> broadcast signed'
                            Nothing -> logError $ "Failed to sign relay list metadata event"
                        return True

    RemoveGeneralRelay pk r -> do
        let r' = normalizeRelayURI r
        disconnectRelay r'
        modify $ \st -> st 
            { generalRelays = Map.adjust (removeRelayFromList r') pk (generalRelays st) }
        updatedRelays <- gets (Map.findWithDefault ([], 0) pk . generalRelays)
        updateRelays (AccountId $ pubKeyXOToBech32 pk) updatedRelays
        notifyRelayStatus
        kp <- getKeyPair
        st <- get @RelayPoolState
        let (rs, _) = Map.findWithDefault ([], 0) pk (generalRelays st)
        now <- getCurrentTime
        let unsigned = createRelayListMetadataEvent rs pk now
        signed <- signEvent unsigned kp
        case signed of
            Just signed' -> broadcast signed'
            Nothing -> logError $ "Failed to sign relay list metadata event"

    GetGeneralRelays pk -> do
        st <- get @RelayPoolState
        let (relays, timestamp) = Map.findWithDefault ([], 0) pk (generalRelays st)
        relaysWithStatus <- forM relays $ \relay -> do
            let uri = getUri relay
            let status = case Map.lookup uri (activeConnections st) of
                    Just rd -> connectionState rd
                    Nothing -> Disconnected
            return (relay, status)
        return (relaysWithStatus, timestamp)

    ImportDMRelays pk rs ts -> do
        let rs' = map normalizeRelay rs
        modify @RelayPoolState $ \st -> do
            case Map.lookup pk (dmRelays st) of
                Nothing -> st { dmRelays = Map.insert pk (rs', ts) (dmRelays st) }
                Just (_, existingTs) -> 
                    if ts > existingTs
                        then st { dmRelays = Map.insert pk (rs', ts) (dmRelays st) }
                        else st
        notifyRelayStatus

    AddDMRelay pk r -> do
        st <- get @RelayPoolState
        let (existingRelays, _) = Map.findWithDefault ([], 0) pk (dmRelays st)
        let newRelay = InboxOutboxRelay $ normalizeRelayURI r
        if newRelay `elem` existingRelays
            then return False
            else do
                now <- getCurrentTime
                modify @RelayPoolState $ \st' -> st'
                    { dmRelays = Map.insert pk (newRelay : existingRelays, now) (dmRelays st') }
                notifyRelayStatus
                kp <- getKeyPair
                let unsigned = createPreferredDMRelaysEvent (map getUri $ newRelay : existingRelays) pk now
                signed <- signEvent unsigned kp
                case signed of
                    Just signed' -> broadcast signed'
                    Nothing -> logError $ "Failed to sign preferred DM relays event"
                return True

    RemoveDMRelay pk r -> do
        let r' = normalizeRelayURI r
        modify @RelayPoolState $ \st -> st
            { dmRelays = Map.adjust (removeRelayFromList r') pk (dmRelays st) }
        notifyRelayStatus
        kp <- getKeyPair
        st <- get @RelayPoolState
        let (rs, _) = Map.findWithDefault ([], 0) pk (dmRelays st)
        now <- getCurrentTime
        let unsigned = createPreferredDMRelaysEvent (map getUri rs) pk now
        signed <- signEvent unsigned kp
        case signed of
            Just signed' -> broadcast signed'
            Nothing -> logError $ "Failed to sign preferred DM relays event"

    GetDMRelays pk -> do
        st <- get @RelayPoolState
        let (relays, timestamp) = Map.findWithDefault ([], 0) pk (dmRelays st)
        relaysWithStatus <- forM relays $ \relay -> do
            let uri = getUri relay
            let status = case Map.lookup uri (activeConnections st) of
                    Just rd -> connectionState rd
                    Nothing -> Disconnected
            return (relay, status)
        return (relaysWithStatus, timestamp)


-- | Normalize a Relay by normalizing its URI
normalizeRelay :: Relay -> Relay
normalizeRelay relay = case relay of
    InboxRelay uri -> InboxRelay (normalizeRelayURI uri)
    OutboxRelay uri -> OutboxRelay (normalizeRelayURI uri)
    InboxOutboxRelay uri -> InboxOutboxRelay (normalizeRelayURI uri)


-- | Remove a specific relay from a relay list without affecting other relays
removeRelayFromList :: RelayURI -> ([Relay], Int) -> ([Relay], Int)
removeRelayFromList uri (relays, timestamp) =
    (filter (\relay -> getUri relay /= uri) relays, timestamp)
