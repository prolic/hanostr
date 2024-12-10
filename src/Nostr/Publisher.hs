module Nostr.Publisher where

import Control.Monad (forM_, when)
import Data.List (nub, partition)
import Data.Map.Strict qualified as Map
import Data.Text (Text, pack)
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (atomically, writeTChan)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, modify)
import Effectful.TH
--import Network.URI (URI(..), parseURI, uriAuthority, uriPort, uriRegName, uriScheme)

import QtQuick
import Logging
import Nostr.Bech32 (pubKeyXOToBech32)
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.RelayConnection
import Nostr.Types (Event(..), EventId, Relay(..), RelayURI, Request(..), getUri)
import Nostr.Util
import Types ( AppState(..), ConnectionState(..), Follow(..)
             , PublishStatus(..), RelayData(..), RelayPoolState(..) )


-- | Result of a publish operation
data PublishResult
    = PublishSuccess [RelayURI] [RelayURI]
    | PublishFailure Text
    deriving (Show)


-- | Publisher effects
data Publisher :: Effect where
    Broadcast :: Event -> Publisher m ()
    PublishToOutbox :: Event -> Publisher m ()
    PublishToRelay :: Event -> RelayURI -> Publisher m ()
    PublishGiftWrap :: Event -> PubKeyXO -> Publisher m ()
    GetPublishResult :: EventId -> Publisher m PublishResult

type instance DispatchOf Publisher = Dynamic

makeEffect ''Publisher


-- | Publisher effect handlers
type PublisherEff es =
  ( State AppState :> es
  , State RelayPoolState :> es
  , RelayConnection :> es
  , QtQuick :> es
  , Concurrent :> es
  , Logging :> es
  , Util :> es
  )


-- | Run a publisher
runPublisher
  :: PublisherEff es
  => Eff (Publisher : es) a
  -> Eff es a
runPublisher =  interpret $ \_ -> \case
    Broadcast event -> do  
        allDMRelays <- gets @RelayPoolState (concatMap (fst . snd) . Map.toList . dmRelays)
        
        myGeneralRelays <- gets @RelayPoolState (Map.findWithDefault ([], 0) (pubKey event) . generalRelays)
        let (myOutboxCapable, _) = partition isOutboxCapable (fst myGeneralRelays)
        
        kp <- getKeyPair
        let pk = keyPairToPubKeyXO kp
        st <- get @AppState
        let follows' = Map.findWithDefault [] pk (follows st)
        let followPks = map pubkey follows'
        otherGeneralRelays <- gets @RelayPoolState (concatMap (fst . snd) . 
            filter (\(k,_) -> k `elem` followPks && k /= pubKey event) . Map.toList . generalRelays)
        
        let allTargetRelays = nub $ 
                map getUri allDMRelays ++
                map getUri myOutboxCapable ++
                map getUri otherGeneralRelays

        modify $ \st' -> st' 
            { publishStatus = Map.insert 
                (eventId event) 
                (Map.fromList [(relay, Publishing) | relay <- allTargetRelays]) 
                (publishStatus st') 
            }
        
        existingConnections <- getConnectedRelays
        let (existingRelays, newRelays) = partition (`elem` existingConnections) allTargetRelays
        
        forM_ existingRelays $ \r -> writeToChannel event r

        forM_ newRelays $ \r -> async $ do
            connected <- connectRelay r
            when connected $ do
                writeToChannel event r
                disconnectRelay r

    PublishToOutbox event -> do
        kp <- getKeyPair
        let pk = keyPairToPubKeyXO kp
        generalRelayList <- gets (Map.findWithDefault ([], 0) pk . generalRelays)
        let (outboxRelays, _) = generalRelayList
            outboxCapableURIs = map getUri $ filter isOutboxCapable outboxRelays

        modify $ \st -> st 
            { publishStatus = Map.insert 
                (eventId event) 
                (Map.fromList [(relay, Publishing) | relay <- outboxCapableURIs]) 
                (publishStatus st) 
            }

        forM_ outboxCapableURIs $ \r -> writeToChannel event r

    PublishToRelay event' relayUri' -> do
        modify $ \st -> st 
            { publishStatus = Map.adjust 
                (\existingMap -> Map.insert relayUri' Publishing existingMap)
                (eventId event')
                (publishStatus st) 
            }
        writeToChannel event' relayUri'

    PublishGiftWrap event' senderPk -> do
        logDebug $ "Publishing gift wrap"
        -- Log subscription details
        logDebug $ "Publishing gift wrap event: " <> pack (show $ eventId event')
        logDebug $ "Sender pubkey: " <> pubKeyXOToBech32 senderPk
        logDebug $ "Recipient pubkey: " <> pubKeyXOToBech32 (pubKey event')
        -- Get sender and recipient relay lists
        dmRelayList <- gets @RelayPoolState (Map.findWithDefault ([], 0) senderPk . dmRelays)
        recipientDMRelays <- gets @RelayPoolState (Map.findWithDefault ([], 0) (pubKey event') . dmRelays)

        if null dmRelayList || null recipientDMRelays
            then pure ()
            else do
                let allRelayURIs = nub $ 
                        map getUri (fst dmRelayList) ++ 
                        map getUri (fst recipientDMRelays)

                -- Split relays into existing connections and new ones
                existingConnections <- getConnectedRelays
                let (existingRelays, newRelays) = partition 
                        (`elem` existingConnections) 
                        allRelayURIs

                -- Handle existing connections
                forM_ existingRelays $ \r -> writeToChannel event' r
                
                -- Handle new connections
                forM_ newRelays $ \r -> async $ do
                    connected <- connectRelay r
                    when connected $ do
                        writeToChannel event' r
                        disconnectRelay r

    GetPublishResult eventId' -> do
        st <- get @RelayPoolState
        let states = Map.findWithDefault Map.empty eventId' (publishStatus st)
        if null states
            then return $ PublishFailure "No relays found to publish to"
            else do
                let (successRelays, failureRelays) = Map.partitionWithKey (\_ v -> v == Success) states
                return $ PublishSuccess (Map.keys successRelays) (Map.keys failureRelays)


-- | Write an event to a channel
writeToChannel :: PublisherEff es => Event -> RelayURI -> Eff es ()
writeToChannel e r = do
    st <- get @RelayPoolState
    case Map.lookup r (activeConnections st) of
        Just rd -> do
            atomically $ writeTChan (requestChannel rd) (SendEvent e)
            modify $ \st' -> st' { publishStatus = Map.adjust (Map.insert r WaitingForConfirmation) (eventId e) (publishStatus st') }
        Nothing -> do
            modify $ \st' -> st' { publishStatus = Map.adjust (Map.insert r (Failure "No channel found")) (eventId e) (publishStatus st') }


-- | Get the connected relays
getConnectedRelays :: PublisherEff es => Eff es [RelayURI]
getConnectedRelays = do
    st <- get @RelayPoolState
    return $ Map.keys $ Map.filter ((== Connected) . connectionState) (activeConnections st)


-- | Check if a relay is outbox capable
isOutboxCapable :: Relay -> Bool
isOutboxCapable (OutboxRelay _) = True
isOutboxCapable (InboxOutboxRelay _) = True
isOutboxCapable _ = False
