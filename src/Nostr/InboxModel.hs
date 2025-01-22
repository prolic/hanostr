{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Nostr.InboxModel where

import Control.Monad (forM, forM_, unless, void, when)
import Data.Either (partitionEithers)
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set qualified as Set
import Data.Text (pack, unpack)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async, forConcurrently)
import Effectful.Concurrent.STM
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, put, modify)
import Effectful.TH

import Logging
import Nostr
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.RelayConnection (RelayConnection, connectRelay, disconnectRelay)
import Nostr.Subscription 
    ( Subscription
    , giftWrapFilter
    , handleEvent
    , mentionsFilter
    , newSubscriptionId
    , profilesFilter
    , stopAllSubscriptions
    , stopSubscription
    , subscribe
    , userPostsFilter
    )
import Nostr.Types
  ( RelayURI
  , Relay(..)
  , Event(..)
  , Filter(..)
  , Kind(..)
  , SubscriptionId
  , getUri
  , isInboxCapable
  , isOutboxCapable
  , defaultGeneralRelays
  )
import Nostr.Util
import QtQuick (QtQuick, notify)
import RelayMgmt
import Store.Lmdb (LmdbStore, getFollows, getGeneralRelays, getDMRelays, getLatestTimestamp)
import Types (AppState(..), ConnectionState(..), Follow(..), SubscriptionDetails(..), RelayPool(..), RelayData(..), SubscriptionEvent(..), initialRelayPool)

-- | InboxModel effects
data InboxModel :: Effect where
  StartInboxModel :: InboxModel m ()
  StopInboxModel :: InboxModel m ()
  AwaitAtLeastOneConnected :: InboxModel m Bool

type instance DispatchOf InboxModel = Dynamic

makeEffect ''InboxModel

type InboxModelEff es =
  ( State AppState :> es
  , State RelayPool :> es
  , LmdbStore :> es
  , Subscription :> es
  , RelayConnection :> es
  , RelayMgmt :> es
  , Logging :> es
  , Concurrent :> es
  , Util :> es
  , Nostr :> es
  , QtQuick :> es
  )

-- | Run InboxModel
runInboxModel :: InboxModelEff es => Eff (InboxModel : es) a -> Eff es a
runInboxModel = interpret $ \_ -> \case
  StartInboxModel -> do
    kp <- getKeyPair
    let xo = keyPairToPubKeyXO kp
    queue <- newTQueueIO
    modify @RelayPool (\s -> s { inboxQueue = queue })
    void $ async $ do
      initializeSubscriptions xo
      eventLoop xo

  StopInboxModel -> do
    st <- get @RelayPool
    forM_ (Map.keys $ activeConnections st) $ \relayUri -> do
      disconnectRelay relayUri
    put @RelayPool initialRelayPool

  AwaitAtLeastOneConnected -> awaitAtLeastOneConnected'

-- | Wait until at least one relay is connected
awaitAtLeastOneConnected' :: InboxModelEff es => Eff es Bool
awaitAtLeastOneConnected' = do
  let loop = do
        st <- get @RelayPool
        let states = map (connectionState . snd) $ Map.toList $ activeConnections st
        if any (== Connected) states
            then return True
            else if null states
              then do
                threadDelay 50000  -- 50ms delay
                loop
              else if all (== Disconnected) states
                  then return False
                  else do
                      threadDelay 50000  -- 50ms delay
                      loop
  loop

-- | Initialize subscriptions
initializeSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
initializeSubscriptions xo = do
  follows <- getFollows xo
  let followList = map pubkey follows

  inboxRelays <- getGeneralRelays xo

  if null inboxRelays
    then initializeWithDefaultRelays xo followList
    else initializeWithExistingRelays xo followList inboxRelays

-- | Initialize using default relays when no relay configuration exists
initializeWithDefaultRelays :: InboxModelEff es => PubKeyXO -> [PubKeyXO] -> Eff es ()
initializeWithDefaultRelays xo followList = do
  let (defaultRelays, _) = defaultGeneralRelays

  connectionResults <- forConcurrently defaultRelays $ \relay -> do
    connected <- connectRelay (getUri relay)
    return (relay, connected)

  void $ awaitAtLeastOneConnected'

  let connectedRelays = [relay | (relay, success) <- connectionResults, success]

  initQueue <- newTQueueIO
  let filter' = profilesFilter [xo] Nothing

  subIdsResult <- subscribeToFilter connectedRelays filter' initQueue
  case subIdsResult of
    Right subIds -> do
      logDebug "Looking for relay data on default relays..."
      receivedEvents <- collectEventsUntilEose initQueue
      forM_ subIds stopSubscription

      forM_ receivedEvents $ \(r, e') -> do
        case e' of
          EventAppeared e'' -> do
            updates <- handleEvent r e''
            notify updates
          _ -> return ()

      let hasRelayMeta = hasRelayListMetadata $ map snd receivedEvents
          hasPreferredDM = hasPreferredDMRelays $ map snd receivedEvents

      unless hasRelayMeta $ do
        logInfo "RelayListMetadata event not found. Setting default general relays."
        setDefaultGeneralRelays xo

      unless hasPreferredDM $ do
        logInfo "PreferredDMRelays event not found. Setting default DM relays."
        setDefaultDMRelays xo

      -- Continue with appropriate relays
      inboxRelays <- getGeneralRelays xo
      dmRelays <- getDMRelays xo
      continueWithRelays followList inboxRelays dmRelays
    Left err -> logError $ "Could not look for relay data on default relays: " <> pack err

-- | Initialize with existing relay configuration
initializeWithExistingRelays :: InboxModelEff es => PubKeyXO -> [PubKeyXO] -> [Relay] -> Eff es ()
initializeWithExistingRelays xo followList inboxRelays = do
  dmRelays <- getDMRelays xo
  continueWithRelays followList inboxRelays dmRelays

-- | Subscribe to a filter on multiple relays
subscribeToFilter
  :: InboxModelEff es
  => [Relay]
  -> Filter
  -> TQueue (RelayURI, SubscriptionEvent)
  -> Eff es (Either String [SubscriptionId])
subscribeToFilter relays f queue = do
  results <- forM relays $ \relay -> do
    let relayUri = getUri relay
    subId <- newSubscriptionId
    result <- subscribe relayUri subId f queue
    return $ case result of
      Right () -> Right (relayUri, subId)
      Left err -> Left err

  -- Collect successful subscriptions
  return $ case partitionEithers results of
    ([], subs) -> Right (map snd subs)
    (errs, _) -> Left (unlines errs)

-- | Collect events until EOSE is received
collectEventsUntilEose :: InboxModelEff es => TQueue (RelayURI, SubscriptionEvent) -> Eff es [(RelayURI, SubscriptionEvent)]
collectEventsUntilEose queue = do
  let loop acc = do
        event <- atomically $ readTQueue queue
        case snd event of
          SubscriptionEose -> return acc
          SubscriptionClosed _ -> return acc
          _ -> loop (event : acc)
  loop []

-- | Check if RelayListMetadata event is present
hasRelayListMetadata :: [SubscriptionEvent] -> Bool
hasRelayListMetadata events = any isRelayListMetadata events
  where
    isRelayListMetadata (EventAppeared event') = kind event' == RelayListMetadata
    isRelayListMetadata _ = False

-- | Check if PreferredDMRelays event is present
hasPreferredDMRelays :: [SubscriptionEvent] -> Bool
hasPreferredDMRelays events = any isPreferredDMRelays events
  where
    isPreferredDMRelays (EventAppeared event') = kind event' == PreferredDMRelays
    isPreferredDMRelays _ = False

-- | Continue with discovered relays
continueWithRelays :: InboxModelEff es => [PubKeyXO] -> [Relay] -> [Relay] -> Eff es ()
continueWithRelays followList inboxRelays dmRelays = do
  kp <- getKeyPair
  let xo = keyPairToPubKeyXO kp
  let ownInboxRelayURIs = [ getUri relay | relay <- inboxRelays, isInboxCapable relay ]
  logDebug $ "Initializing subscriptions for Discovered Inbox Relays: " <> pack (show ownInboxRelayURIs)
  logDebug $ "Initializing subscriptions for Discovered DM Relays: " <> pack (show (map getUri dmRelays))

  -- Connect to DM relays concurrently
  void $ forConcurrently dmRelays $ \relay -> do
    let relayUri = getUri relay
    connected <- connectRelay relayUri
    if connected
      then do
        logInfo $ "Connected to DM Relay: " <> relayUri
        subscribeToGiftwraps relayUri xo
        logInfo $ "Subscribed to Giftwraps on relay: " <> relayUri
      else
        logError $ "Failed to connect to DM Relay: " <> relayUri

  -- Connect to inbox relays concurrently
  void $ forConcurrently inboxRelays $ \relay -> do
    let relayUri = getUri relay
    connected <- connectRelay relayUri
    if connected
      then do
        logInfo $ "Connected to Inbox Relay: " <> relayUri
        when (isInboxCapable relay) $ do
          subscribeToMentions relayUri xo
          logInfo $ "Subscribed to Mentions on relay: " <> relayUri
      else
        logError $ "Failed to connect to Inbox Relay: " <> relayUri

  followRelayMap <- buildRelayPubkeyMap followList ownInboxRelayURIs
  logDebug $ "Building Relay-PubKey Map: " <> pack (show followRelayMap)

  -- Connect to follow relays concurrently
  void $ forConcurrently (Map.toList followRelayMap) $ \(relayUri, pubkeys) -> do
    connected <- connectRelay relayUri
    if connected
      then do
        logInfo $ "Connected to Follow Relay: " <> relayUri
        subscribeToRelay relayUri pubkeys
        logInfo $ "Subscribed to Relay: " <> relayUri <> " for PubKeys: " <> pack (show pubkeys)
      else
        logError $ "Failed to connect to Follow Relay: " <> relayUri

-- | Subscribe to Giftwrap events on a relay
subscribeToGiftwraps :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToGiftwraps relayUri xo = do
  subId <- newSubscriptionId
  lastTimestamp <- getSubscriptionTimestamp [xo] [GiftWrap]
  queue <- gets @RelayPool inboxQueue
  result <- subscribe relayUri subId (giftWrapFilter xo lastTimestamp) queue
  case result of
    Right () -> logDebug $ "Subscribed to giftwraps on " <> relayUri
    Left err -> logWarning $ "Failed to subscribe to giftwraps on " <> relayUri <> ": " <> pack err

-- | Subscribe to mentions on a relay
subscribeToMentions :: InboxModelEff es => RelayURI -> PubKeyXO -> Eff es ()
subscribeToMentions relayUri xo = do
  subId <- newSubscriptionId
  lastTimestamp <- getSubscriptionTimestamp [xo] [ShortTextNote, Repost, Comment, EventDeletion]
  queue <- gets @RelayPool inboxQueue
  result <- subscribe relayUri subId (mentionsFilter xo lastTimestamp) queue
  case result of
    Right () -> logDebug $ "Subscribed to mentions on " <> relayUri
    Left err -> logWarning $ "Failed to subscribe to mentions on " <> relayUri <> ": " <> pack err


-- | Subscribe to profiles and posts for a relay
subscribeToRelay :: InboxModelEff es => RelayURI -> [PubKeyXO] -> Eff es ()
subscribeToRelay relayUri pks = do
  -- Subscribe to profiles
  profileSubId         <- newSubscriptionId
  profileLastTimestamp <- getSubscriptionTimestamp pks [RelayListMetadata, PreferredDMRelays, FollowList]
  queue                <- gets @RelayPool inboxQueue
  profileResult        <- subscribe relayUri profileSubId (profilesFilter pks profileLastTimestamp) queue
  case profileResult of
    Right () -> logDebug $ "Subscribed to profiles on " <> relayUri
    Left err -> logWarning $ "Failed to subscribe to profiles on " <> relayUri <> ": " <> pack err

  -- Subscribe to posts
  postsSubId         <- newSubscriptionId
  postsLastTimestamp <- getSubscriptionTimestamp pks [ShortTextNote, Repost, EventDeletion]
  postsResult        <- subscribe relayUri postsSubId (userPostsFilter pks postsLastTimestamp) queue
  case postsResult of
    Right () -> logDebug $ "Subscribed to posts on " <> relayUri
    Left err -> logWarning $ "Failed to subscribe to posts on " <> relayUri <> ": " <> pack err


getSubscriptionTimestamp :: InboxModelEff es => [PubKeyXO] -> [Kind] -> Eff es (Maybe Int)
getSubscriptionTimestamp pks ks = do
  timestamps <- forM pks $ \pk -> getLatestTimestamp pk ks
  if null timestamps 
    then return Nothing
    else case catMaybes timestamps of
      [] -> return Nothing
      ts -> return $ Just $ minimum ts

-- | Build a map from relay URI to pubkeys, prioritizing existing inbox relays
buildRelayPubkeyMap :: InboxModelEff es => [PubKeyXO] -> [RelayURI] -> Eff es (Map.Map RelayURI [PubKeyXO])
buildRelayPubkeyMap pks ownInboxRelays = do
  relayPubkeyPairs <- forM pks $ \pk -> do
    relays <- getGeneralRelays pk
    let outboxRelayURIs = [ getUri relay | relay <- relays, isOutboxCapable relay ]
    let (prioritized, other) = partition (`elem` ownInboxRelays) outboxRelayURIs
    let selectedRelays = take maxRelaysPerContact $ prioritized ++ other
    return (pk, selectedRelays)

  return $ Map.filter (not . null) $ foldr (\(pk, relays) acc ->
    foldr (\relay acc' -> Map.insertWith (++) relay [pk] acc') acc relays
    ) Map.empty relayPubkeyPairs

-- | Maximum number of outbox relays to consider per contact
maxRelaysPerContact :: Int
maxRelaysPerContact = 3

-- | Event loop to handle incoming events and updates
eventLoop :: InboxModelEff es => PubKeyXO -> Eff es ()
eventLoop xo = do
  shouldStopVar <- newTVarIO False
  let loop = do
        events <- collectEvents

        forM_ events $ \(relayUri, event) -> do
          case event of
            EventAppeared event' -> do
              updates <- handleEvent relayUri event'

              when (kind event' == FollowList || kind event' == PreferredDMRelays || kind event' == RelayListMetadata) $ do
                logInfo "Detected updated FollowList, PreferredDMRelays, or RelayListMetadata; updating subscriptions."
                updateSubscriptions xo

              notify updates
            SubscriptionEose -> return ()
            SubscriptionClosed _ -> atomically $ writeTVar shouldStopVar True

        shouldStop <- atomically $ readTVar shouldStopVar
        unless shouldStop loop
  loop

-- | Collect events from all active subscriptions
collectEvents :: InboxModelEff es => Eff es [(RelayURI, SubscriptionEvent)]
collectEvents = do
  queue <- gets @RelayPool inboxQueue
  atomically $ do
    event <- readTQueue queue
    remainingEvents <- flushTQueue queue
    return (event : remainingEvents)

-- | Update subscriptions when follows or preferred DM relays change
updateSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateSubscriptions xo = do
  updateGeneralSubscriptions xo
  updateDMSubscriptions xo

-- | Update general subscriptions based on current follow list
updateGeneralSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateGeneralSubscriptions xo = do
  follows <- getFollows xo
  let followList = map pubkey follows

  inboxRelays <- getGeneralRelays xo
  let ownInboxRelayURIs = [ getUri relay | relay <- inboxRelays, isInboxCapable relay ]

  void $ forConcurrently inboxRelays $ \relay -> do
    when (isInboxCapable relay) $ do
      let relayUri = getUri relay
      connected <- connectRelay relayUri
      when connected $ subscribeToMentions relayUri xo

  newRelayPubkeyMap <- buildRelayPubkeyMap followList ownInboxRelayURIs

  st <- get @RelayPool
  let currentRelays = Map.keysSet (activeConnections st)
  let newRelays = Map.keysSet newRelayPubkeyMap

  let relaysToAdd = Set.difference newRelays currentRelays
  let relaysToRemove = Set.difference currentRelays newRelays
  let relaysToUpdate = Set.intersection currentRelays newRelays

  void $ forConcurrently (Set.toList relaysToRemove) $ \relayUri -> do
    disconnectRelay relayUri

  void $ forConcurrently (Set.toList relaysToAdd) $ \relayUri -> do
    let pubkeys = Map.findWithDefault [] relayUri newRelayPubkeyMap
    connected <- connectRelay relayUri
    when connected $ do
      subscribeToRelay relayUri pubkeys

  void $ forConcurrently (Set.toList relaysToUpdate) $ \relayUri -> do
    let newPubkeys = Set.fromList $ Map.findWithDefault [] relayUri newRelayPubkeyMap
    rd <- getRelayData relayUri
    let currentPubkeys = Set.fromList $ getSubscribedPubkeys rd
    when (newPubkeys /= currentPubkeys) $ do
      stopAllSubscriptions relayUri
      subscribeToRelay relayUri (Set.toList newPubkeys)

-- | Update DM subscriptions
updateDMSubscriptions :: InboxModelEff es => PubKeyXO -> Eff es ()
updateDMSubscriptions xo = do
    kp <- getKeyPair
    let ownPubkey = keyPairToPubKeyXO kp

    when (xo == ownPubkey) $ do
        dmRelays <- getDMRelays xo
        let dmRelayURIs = map getUri dmRelays
        let dmRelaySet = Set.fromList dmRelayURIs

        currentConnections <- gets @RelayPool activeConnections

        let relaysWithGiftwrap = 
              [ (relayUri, rd)
              | (relayUri, rd) <- Map.toList currentConnections
              , any (\sd -> GiftWrap `elem` fromMaybe [] (kinds $ subscriptionFilter sd)) 
                    (Map.elems $ activeSubscriptions rd)
              ]

        void $ forConcurrently relaysWithGiftwrap $ \(relayUri, rd) ->
            unless (relayUri `Set.member` dmRelaySet) $ do
                let giftwrapSubs = 
                      [ subId 
                      | (subId, sd) <- Map.toList (activeSubscriptions rd)
                      , GiftWrap `elem` fromMaybe [] (kinds $ subscriptionFilter sd)
                      ]

                forM_ giftwrapSubs stopSubscription

                let remainingSubCount = Map.size (activeSubscriptions rd) - length giftwrapSubs
                when (remainingSubCount == 0) $ do
                    disconnectRelay relayUri

        let currentRelaySet = Map.keysSet currentConnections
        let relaysToAdd = Set.difference dmRelaySet currentRelaySet

        void $ forConcurrently (Set.toList relaysToAdd) $ \relayUri -> do
            connected <- connectRelay relayUri
            when connected $ do
                subscribeToGiftwraps relayUri xo

-- | Get RelayData for a relay
getRelayData :: InboxModelEff es => RelayURI -> Eff es RelayData
getRelayData relayUri = do
  st <- get @RelayPool
  return $ fromMaybe (error $ "No RelayData for " <> unpack relayUri) $ Map.lookup relayUri (activeConnections st)

-- | Get pubkeys from subscriptions in RelayData
getSubscribedPubkeys :: RelayData -> [PubKeyXO]
getSubscribedPubkeys rd =
  concatMap (fromMaybe [] . authors . subscriptionFilter) (Map.elems $ activeSubscriptions rd)
