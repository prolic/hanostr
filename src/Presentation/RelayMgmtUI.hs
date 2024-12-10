{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Presentation.RelayMgmtUI where

import Control.Monad (void)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Graphics.QML hiding (fireSignal, runEngineLoop)

import QtQuick (QtQuickState(..), UIReferences(..))
import Logging
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.RelayPool
import Nostr.Types hiding (displayName, picture)
import Nostr.Util
import Types (AppState(..), ConnectionState(..), RelayData(..), RelayPoolState(..))


data RelayType = DMRelays | InboxRelays | OutboxRelays

-- | Relay Management UI Effect.
type RelayMgmgtUIEff es =
  ( State AppState :> es
  , State RelayPoolState :> es
  , State QtQuickState :> es
  , RelayPool :> es
  , Logging :> es
  , Concurrent :> es
  , Util :> es
  , IOE :> es
  )

-- | Key Management Effect for creating QML UI.
data RelayMgmtUI :: Effect where
  CreateUI :: SignalKey (IO ()) -> RelayMgmtUI m (ObjRef ())


-- | Dispatch for Key Management UI Effect.
type instance DispatchOf RelayMgmtUI = Dynamic


makeEffect ''RelayMgmtUI


-- | Run the Relay Management UI effect.
runRelayMgmtUI :: RelayMgmgtUIEff es => Eff (RelayMgmtUI : es) a -> Eff es a
runRelayMgmtUI action = interpret handleRelayMgmtUI action
  where
    handleRelayMgmtUI :: RelayMgmgtUIEff es => EffectHandler RelayMgmtUI es
    handleRelayMgmtUI _ = \case
      CreateUI changeKey -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        dmRelayClass <- newClass [
            defPropertySigRO' "url" changeKey $ \obj -> return $ fromObjRef obj,

            defPropertySigRO' "connectionState" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ getConnectionStateText uri' pst,

            defPropertySigRO' "connectionRetries" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> connectionAttempts rd
                Nothing -> 0,

            defPropertySigRO' "notices" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> notices rd
                Nothing -> []
            ]

        relayClass <- newClass [
            defPropertySigRO' "url" changeKey $ \obj -> return $ fromObjRef obj,

            defPropertySigRO' "connectionState" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ getConnectionStateText uri' pst,

            defPropertySigRO' "isInbox" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              outboxState <- get @RelayPoolState
              -- DM relays are always readable/writable
              let isInDMRelays = any (elem uri' . map getUri . fst) (Map.elems $ dmRelays outboxState)
              -- For general relays, check inbox capability
              let isInGeneralRelays = any (any (\r -> isInboxCapable r && getUri r == uri') . fst) (Map.elems $ generalRelays outboxState)
              return $ isInDMRelays || isInGeneralRelays,

            defPropertySigRO' "isOutbox" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              outboxState <- get @RelayPoolState
              let isInDMRelays = any (elem uri' . map getUri . fst) (Map.elems $ dmRelays outboxState)
              let isInGeneralRelays = any (any (\r -> isOutboxCapable r && getUri r == uri') . fst) (Map.elems $ generalRelays outboxState)
              return $ isInDMRelays || isInGeneralRelays,

            defPropertySigRO' "connectionRetries" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> connectionAttempts rd
                Nothing -> 0,

            defPropertySigRO' "notices" changeKey $ \obj -> runE $ do
              let uri' = fromObjRef obj
              pst <- get @RelayPoolState
              return $ case Map.lookup uri' (activeConnections pst) of
                Just rd -> notices rd
                Nothing -> []
            ]

        dmRelayPool <- newFactoryPool (newObject dmRelayClass)
        generalRelayPool <- newFactoryPool (newObject relayClass)
        tempRelayPool <- newFactoryPool (newObject relayClass)

        contextClass <- newClass [
          defPropertySigRO' "dmRelays" changeKey $ \obj -> do
            runE $ modify @QtQuickState $ \s -> s { 
              uiRefs = (uiRefs s) { dmRelaysObjRef = Just obj } 
            }
            appState <- runE $ get @AppState
            case keyPair appState of
              Nothing -> return []
              Just kp -> do
                let pk = keyPairToPubKeyXO kp
                (relaysWithStatus, _) <- runE $ getDMRelays pk
                mapM (\(relay, _status) -> getPoolObject dmRelayPool (getUri relay)) relaysWithStatus,

          defPropertySigRO' "generalRelays" changeKey $ \obj -> do
            runE $ modify @QtQuickState $ \s -> s { 
              uiRefs = (uiRefs s) { generalRelaysObjRef = Just obj } 
            }
            appState <- runE $ get @AppState
            case keyPair appState of
                Nothing -> return []
                Just kp -> do
                    let pk = keyPairToPubKeyXO kp
                    outboxState <- runE $ get @RelayPoolState
                    let rs = case Map.lookup pk (generalRelays outboxState) of
                                Nothing -> []
                                Just (rs', _) -> map getUri rs'
                    mapM (getPoolObject generalRelayPool) rs,

          defPropertySigRO' "tempRelays" changeKey $ \obj -> do
            runE $ modify @QtQuickState $ \s -> s {
              uiRefs = (uiRefs s) { tempRelaysObjRef = Just obj }
            }
            poolState <- runE $ get @RelayPoolState
            appState <- runE $ get @AppState

            let activeURIs = Map.keys (activeConnections poolState)

            case keyPair appState of
              Nothing -> return []
              Just kp -> do
                let pk = keyPairToPubKeyXO kp
                (dmRelaysWithStatus, _) <- runE $ getDMRelays pk
                let dmURIs = map (getUri . fst) dmRelaysWithStatus
                let generalURIs = case Map.lookup pk (generalRelays poolState) of
                                  Nothing -> []
                                  Just (rs, _) -> map getUri rs

                let tempURIs = filter (\uri -> uri `notElem` dmURIs && uri `notElem` generalURIs) activeURIs
                mapM (getPoolObject tempRelayPool) tempURIs,

          defMethod' "addDMRelay" $ \_ input -> runE $ do
            kp <- getKeyPair
            addDMRelay (keyPairToPubKeyXO kp) input,

          defMethod' "removeDMRelay" $ \_ input -> runE $ do
            kp <- getKeyPair
            removeDMRelay (keyPairToPubKeyXO kp) input,

          defMethod' "addGeneralRelay" $ \_ input r w -> runE $ do
            kp <- getKeyPair
            addGeneralRelay (keyPairToPubKeyXO kp) input r w,

          defMethod' "removeGeneralRelay" $ \_ input -> runE $ do
            kp <- getKeyPair
            removeGeneralRelay (keyPairToPubKeyXO kp) input,

          defMethod' "connectRelay" $ \_ input -> runE $ void $ async $ connect input,
          
          defMethod' "disconnectRelay" $ \_ input -> runE $ disconnect input
          ]

        newObject contextClass ()

-- | Check if a relay is outbox capable
-- @todo remove duplicated function
isOutboxCapable :: Relay -> Bool
isOutboxCapable (OutboxRelay _) = True
isOutboxCapable (InboxOutboxRelay _) = True
isOutboxCapable _ = False


-- | Check if a relay is inbox capable
-- @todo remove duplicated function
isInboxCapable :: Relay -> Bool
isInboxCapable (InboxRelay _) = True
isInboxCapable (InboxOutboxRelay _) = True
isInboxCapable _ = False


-- | Helper function to get connection state text
getConnectionStateText :: RelayURI -> RelayPoolState -> Text
getConnectionStateText uri pst = case Map.lookup uri (activeConnections pst) of
  Just rd -> case connectionState rd of
    Connected -> "Connected"
    Disconnected -> "Disconnected"
    Connecting -> "Connecting"
  Nothing -> "Disconnected"
