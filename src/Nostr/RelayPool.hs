{-# LANGUAGE BlockArguments #-}

module Nostr.RelayPool where

import Control.Monad (forM_, when)
import Data.Map.Strict qualified as Map
import Effectful
import Effectful.Concurrent (Concurrent, threadDelay)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get)
import Effectful.TH

import QtQuick
import KeyMgmt (KeyMgmt)
import Logging
import Nostr
import Nostr.GiftWrap
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Publisher
import Nostr.RelayConnection
import Nostr.Subscription
import Nostr.Types (Event(..), Kind(..), Relay(..), RelayURI)
import Nostr.Util
import Types (AppState(..), ConnectionState(..), RelayData(..), RelayPoolState(..))
import RelayMgmt (RelayMgmt)
import RelayMgmt qualified as RM
import Store.Event (EventStore)
import Store.Profile (ProfileStore)


data DisconnectReason = UserInitiated | ConnectionFailure
    deriving (Show, Eq)


-- | Effect for handling RelayPool operations.
data RelayPool :: Effect where
    -- General Relay Management
    ImportGeneralRelays :: PubKeyXO -> [Relay] -> Int -> RelayPool m ()
    AddGeneralRelay :: PubKeyXO -> RelayURI -> Bool -> Bool -> RelayPool m Bool
    RemoveGeneralRelay :: PubKeyXO -> RelayURI -> RelayPool m ()
    GetGeneralRelays :: PubKeyXO -> RelayPool m ([(Relay, ConnectionState)], Int)
    -- DM Relay Management
    ImportDMRelays :: PubKeyXO -> [Relay] -> Int -> RelayPool m ()
    AddDMRelay :: PubKeyXO -> RelayURI -> RelayPool m Bool
    RemoveDMRelay :: PubKeyXO -> RelayURI -> RelayPool m ()
    GetDMRelays :: PubKeyXO -> RelayPool m ([(Relay, ConnectionState)], Int)
    -- Connection Management
    Connect :: RelayURI -> RelayPool m ()
    Disconnect :: RelayURI -> RelayPool m ()
    DisconnectAll :: RelayPool m ()
    AwaitAtLeastOneConnected :: RelayPool m Bool
    -- Event Operations
    SendEvent :: Event -> RelayPool m ()

type instance DispatchOf RelayPool = Dynamic

makeEffect ''RelayPool


-- | RelayPoolEff
type RelayPoolEff es =
  ( State AppState :> es
  , State RelayPoolState :> es
  , EventStore :> es
  , ProfileStore :> es
  , Nostr :> es
  , RelayConnection :> es
  , Publisher :> es
  , RelayMgmt :> es
  , Subscription :> es
  , KeyMgmt :> es
  , GiftWrap :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  , Util :> es
  , IOE :> es
  )


-- | Handler for relay pool effects.
runRelayPool
  :: RelayPoolEff es
  => Eff (RelayPool : es) a
  -> Eff es a
runRelayPool = interpret $ \_ -> \case
    ImportGeneralRelays pk rs ts -> RM.importGeneralRelays pk rs ts
    
    AddGeneralRelay pk relay' r w -> RM.addGeneralRelay pk relay' r w
    
    RemoveGeneralRelay pk r -> RM.removeGeneralRelay pk r

    GetGeneralRelays pk -> RM.getGeneralRelays pk

    ImportDMRelays pk rs ts -> RM.importDMRelays pk rs ts

    AddDMRelay pk r -> RM.addDMRelay pk r

    RemoveDMRelay pk r -> RM.removeDMRelay pk r

    GetDMRelays pk -> RM.getDMRelays pk

    Connect r -> do
        res <- connectRelay r
        when res $ handleRelaySubscription r

    Nostr.RelayPool.Disconnect r -> disconnectRelay r

    DisconnectAll -> do
        st <- get @RelayPoolState
        forM_ (Map.toList $ activeConnections st) $ \(r, _) -> disconnectRelay r

    AwaitAtLeastOneConnected -> do
        let loop = do
                st <- get @RelayPoolState
                let states = map (connectionState . snd) $ Map.toList $ activeConnections st
                if any (== Connected) states
                    then return True
                    else if null states
                        then return False
                        else if all (== Disconnected) states
                            then return False
                            else do
                                threadDelay 50000  -- 50ms delay
                                loop
        loop

    Nostr.RelayPool.SendEvent event -> do
        kp <- getKeyPair
        let pk = keyPairToPubKeyXO kp

        case kind event of
            -- Events that should be broadcast to all relays
            PreferredDMRelays -> broadcast event
            RelayListMetadata -> broadcast event
            Metadata -> broadcast event
            -- Gift wrap events need special handling for DM relays
            GiftWrap -> publishGiftWrap event pk
            -- Default case: publish to outbox-capable relays (FollowList, ShortTextNote, etc.)
            _ -> publishToOutbox event
