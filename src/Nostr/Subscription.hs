{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Nostr.Subscription where

import Control.Monad (unless)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.STM (TQueue, atomically, readTQueue, flushTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, modify)
import Effectful.TH

import EffectfulQML
import Logging
import Nostr.Event (validateEvent)
import Nostr.GiftWrap
import Nostr.Keys (byteStringToHex)
import Nostr.Types (Event(..), EventId(..), Kind(..), RelayURI, Response(..), Tag(..))
import Nostr.RelayPool
import Types (AppState(..), EventConfirmation(..), Follow(..), FollowModel(..), RelayData(..), RelayPoolState(..))


-- Subscription Effects
data Subscription :: Effect where
  HandleResponsesUntilEOSE :: RelayURI -> TQueue Response -> Subscription m ()
  HandleResponsesUntilClosed :: RelayURI -> TQueue Response -> Subscription m ()

type instance DispatchOf Subscription = Dynamic

makeEffect ''Subscription

-- Effectful type for Subscription
type SubscriptionEff es = ( RelayPool :> es
                          , GiftWrap :> es
                          , State RelayPoolState :> es
                          , State AppState :> es
                          , Logging :> es
                          , IOE :> es
                          , Concurrent :> es
                          , EffectfulQML :> es
                          )

-- Run the Subscription effect
runSubscription :: SubscriptionEff es => Eff (Subscription : es) a -> Eff es a
runSubscription = interpret $ \_ -> \case
  HandleResponsesUntilEOSE relayURI' queue -> do
    let loop = do
          msg <- atomically $ readTQueue queue
          msgs <- atomically $ flushTQueue queue
          stopped <- processResponsesUntilEOSE relayURI' (msg : msgs)
          threadDelay $ 100 * 1000 -- 100ms
          unless stopped loop
    loop

  HandleResponsesUntilClosed relayURI' queue -> do
    let loop = do
          msg <- atomically $ readTQueue queue
          msgs <- atomically $ flushTQueue queue
          stopped <- processResponses relayURI' (msg : msgs)
          notifyUI
          threadDelay $ 250 * 1000 -- 250ms
          unless stopped loop
    loop

-- Helper functions

-- | Process responses until EOSE.
processResponsesUntilEOSE :: SubscriptionEff es => RelayURI -> [Response] -> Eff es Bool
processResponsesUntilEOSE _ [] = return False
processResponsesUntilEOSE relayURI' (r:rs) = case r of
  EventReceived _ event' -> do
    handleEvent event' relayURI'
    processResponsesUntilEOSE relayURI' rs
  Eose _ -> return True
  Closed _ _ -> return True
  Ok eventId' accepted' msg -> do
    modify $ handleConfirmation eventId' accepted' msg relayURI'
    processResponsesUntilEOSE relayURI' rs
  Notice msg -> do
    modify $ handleNotice relayURI' msg
    processResponsesUntilEOSE relayURI' rs


-- | Process responses.
processResponses :: SubscriptionEff es => RelayURI -> [Response] -> Eff es Bool
processResponses _ [] = return False
processResponses relayURI' (r:rs) = case r of
  EventReceived _ event' -> do
    handleEvent event' relayURI'
    processResponses relayURI' rs
  Eose subId' -> do
    logDebug $ "EOSE on subscription " <> subId'
    processResponses relayURI' rs
  Closed subId msg -> do
    logDebug $ "Closed subscription " <> subId <> " with message " <> msg
    return True
  Ok eventId' accepted' msg -> do
    logDebug $ "OK on subscription " <> pack ( show eventId' ) <> " with message " <> msg
    modify $ handleConfirmation eventId' accepted' msg relayURI'
    processResponses relayURI' rs
  Notice msg -> do
    modify $ handleNotice relayURI' msg
    processResponses relayURI' rs


-- | Handle an event.
handleEvent :: SubscriptionEff es => Event -> RelayURI -> Eff es ()
handleEvent event' _ =
  if not (validateEvent event')
    then do
      logWarning $ "Invalid event seen: " <> (byteStringToHex $ getEventId (eventId event'))
    else do
      case kind event' of
        Metadata -> case eitherDecode (BSL.fromStrict $ TE.encodeUtf8 $ content event') of
          Right profile -> do
            modify $ \st ->
              st { profiles = Map.insertWith (\new old -> if snd new > snd old then new else old)
                                      (pubKey event')
                                      (profile, createdAt event')
                                      (profiles st)
                }
          Left err -> logWarning $ "Failed to decode metadata: " <> pack err

        FollowList -> do
          let followList' = [Follow pk relayUri' displayName' | PTag pk relayUri' displayName' <- tags event']
          modify $ \st -> st { follows = FollowModel (Map.insert (pubKey event') followList' (followList $ follows st)) (objRef $ follows st) }

        GiftWrap -> handleGiftWrapEvent event'

        _ -> logDebug $ "Ignoring gift wrapped event of kind: " <> pack (show (kind event'))


-- | Handle a notice.
handleNotice :: RelayURI -> Text -> RelayPoolState -> RelayPoolState
handleNotice relayURI' msg st =
  st { relays = Map.adjust (\rd -> rd { notices = msg : notices rd }) relayURI' (relays st) }


-- | Handle a confirmation.
handleConfirmation :: EventId -> Bool -> Text -> RelayURI -> AppState -> AppState
handleConfirmation eventId' accepted' msg relayURI' st =
  let updateConfirmation = EventConfirmation
        { relay = relayURI'
        , waitingForConfirmation = False
        , accepted = accepted'
        , message = msg
        }

      updateConfirmations :: [EventConfirmation] -> [EventConfirmation]
      updateConfirmations [] = [updateConfirmation]
      updateConfirmations (conf:confs)
        | relay conf == relayURI' && waitingForConfirmation conf =
            updateConfirmation : confs
        | otherwise = conf : updateConfirmations confs
  in st  { confirmations = Map.alter
         (\case
           Nothing -> Just [updateConfirmation]
           Just confs -> Just $ updateConfirmations confs)
         eventId'
         (confirmations st)
      }
