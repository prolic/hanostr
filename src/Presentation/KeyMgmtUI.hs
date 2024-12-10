{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Presentation.KeyMgmtUI where

import Control.Monad (forM_, void, when)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Dispatch.Dynamic (EffectHandler, interpret)
import Effectful.FileSystem (FileSystem)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH
import Graphics.QML hiding (fireSignal, runEngineLoop)
import System.Random.Shuffle (shuffleM)

import QtQuick
import KeyMgmt
import Logging
import Nostr
import Nostr.Bech32
import Nostr.Event (createPreferredDMRelaysEvent, createRelayListMetadataEvent)
import Nostr.Keys (keyPairToPubKeyXO)
import Nostr.Publisher
import Nostr.RelayPool
import Nostr.Types hiding (displayName, picture)
import Nostr.Util
import Types (AppState(..), RelayPoolState(..), initialRelayPoolState)

-- | Key Management UI Effect.
type KeyMgmgtUIEff es = (State AppState :> es, State RelayPoolState :> es, Util :> es, Nostr :> es, Publisher :> es, KeyMgmt :> es, RelayPool :> es, Concurrent :> es, State KeyMgmtState :> es, IOE :> es, QtQuick :> es, FileSystem :> es, Logging :> es)

-- | Key Management Effect for creating QML UI.
data KeyMgmtUI :: Effect where
  CreateUI :: SignalKey (IO ()) -> KeyMgmtUI m (ObjRef ())


-- | Dispatch for Key Management UI Effect.
type instance DispatchOf KeyMgmtUI = Dynamic


makeEffect ''KeyMgmtUI


-- | Run the Key Management UI effect.
runKeyMgmtUI :: KeyMgmgtUIEff es => Eff (KeyMgmtUI : es) a -> Eff es a
runKeyMgmtUI action = interpret handleKeyMgmtUI action
  where
    handleKeyMgmtUI :: KeyMgmgtUIEff es => EffectHandler KeyMgmtUI es
    handleKeyMgmtUI _ = \case
      CreateUI changeKey -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
        runE loadAccounts

        let prop n f = defPropertySigRO' n changeKey ( \obj -> runE $ do
              st <- get
              let res = maybe "" f $ Map.lookup (fromObjRef obj) (accountMap st)
              return res)

        let mprop n f = defPropertySigRO' n changeKey ( \obj -> runE $ do
              st <- get
              let res = case Map.lookup (fromObjRef obj) (accountMap st) of
                    Just acc -> f acc
                    Nothing -> Nothing
              return res)

        accountClass <-
          newClass
            [ prop "nsec" (secKeyToBech32 . accountSecKey),
              prop "npub" (pubKeyXOToBech32 . accountPubKeyXO),
              mprop "displayName" accountDisplayName,
              mprop "picture" accountPicture
            ]

        accountPool' <- newFactoryPool (newObject accountClass)

        runE $ modify $ \st -> st {accountPool = Just accountPool'}

        contextClass <-
          newClass
            [ defPropertySigRO' "accounts" changeKey $ \_ -> do
                st <- runE get
                mapM (getPoolObject accountPool') $ Map.keys (accountMap st),
              defMethod' "removeAccount" $ \obj input -> runE $ removeAccount obj input,
              defPropertySigRO' "seedphrase" changeKey $ \_ -> do
                st <- runE get
                return $ seedphrase st,
              defPropertySigRO' "nsec" changeKey $ \_ -> do
                st <- runE get
                return $ nsecView st,
              defPropertySigRO' "npub" changeKey $ \_ -> do
                st <- runE get
                return $ npubView st,
              defPropertySigRW'
                "errorMsg"
                changeKey
                ( \_ -> do
                    st <- runE get
                    return $ errorMsg st
                )
                ( \obj newErrorMsg -> runE $ do
                    modify $ \st -> st {errorMsg = newErrorMsg}
                    fireSignal obj
                    return ()
                ),
              defMethod' "importSecretKey" $ \obj (input :: Text) -> runE $ importSecretKey obj input,
              defMethod' "importSeedphrase" $ \obj input pwd -> runE $ importSeedphrase obj input pwd,
              defMethod' "createAccount" $ \obj -> runE $ do
                mkp <- generateSeedphrase obj
                case mkp of
                  Just kp -> do
                    let pk = keyPairToPubKeyXO kp
                    modify @AppState $ \s -> s { keyPair = Just kp }
                    modify @RelayPoolState $ const initialRelayPoolState
                    now <- getCurrentTime

                    -- Import selected general relays
                    let (rs, _) = defaultGeneralRelays
                    allRelays <- liftIO $ shuffleM rs
                    let selectedRelays = take 3 allRelays
                    importGeneralRelays pk selectedRelays now

                    -- Import DM relays first
                    let (dmRelays', _) = defaultDMRelays
                    importDMRelays pk dmRelays' now

                    -- Connect to all relays at once
                    let allRelaysToConnect = selectedRelays ++ dmRelays'
                    forM_ allRelaysToConnect $ \relay -> void $ async $ connect $ getUri relay

                    -- Wait for connection and publish events
                    void $ async $ do
                        atLeastOneConnected <- awaitAtLeastOneConnected
                        when atLeastOneConnected $ do
                            threadDelay 100000 -- wait another 100ms for other relays to connect
                            -- Broadcast relay list metadata
                            let unsigned = createRelayListMetadataEvent selectedRelays pk now
                            signed <- signEvent unsigned kp
                            case signed of
                                Just signed' -> broadcast signed'
                                Nothing -> logError "Failed to sign relay list metadata event"

                            -- Broadcast preferred DM relays event
                            let unsigned' = createPreferredDMRelaysEvent (map getUri dmRelays') pk now
                            signed' <- signEvent unsigned' kp
                            case signed' of
                                Just signed'' -> broadcast signed''
                                Nothing -> logError "Failed to sign preferred DM relays event"

                    return True

                  Nothing -> do
                    logError "Failed to get keypair after generating seedphrase"
                    return False

            ]

        newObject contextClass ()

-- | Check if a relay is inbox capable
-- @todo remove duplicated function
isInboxCapable :: Relay -> Bool
isInboxCapable (InboxRelay _) = True
isInboxCapable (InboxOutboxRelay _) = True
isInboxCapable _ = False


-- | Check if a relay is outbox capable
isOutboxCapable :: Relay -> Bool
isOutboxCapable (OutboxRelay _) = True
isOutboxCapable (InboxOutboxRelay _) = True
isOutboxCapable _ = False
