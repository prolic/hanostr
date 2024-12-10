{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Nostr.GiftWrap where

import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, modify)
import Effectful.TH (makeEffect)
import Data.List (sort, sortBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import Logging
import Nostr
import Nostr.Event (validateEvent)
import Nostr.Keys (KeyPair, PubKeyXO, byteStringToHex, keyPairToPubKeyXO)
import Nostr.Types (Event(..), EventId(..), Kind(..), Rumor(..), Tag(..))
import Nostr.Util (Util, getCurrentTime)
import TimeFormatter (Language(..), formatDateTime)
import Types (AppState(..),ChatMessage(..))

-- | GiftWrap Effects.
data GiftWrap :: Effect where
  HandleGiftWrapEvent :: Event -> GiftWrap m ()

-- | Dispatch type for GiftWrap effect.
type instance DispatchOf GiftWrap = Dynamic


makeEffect ''GiftWrap


-- | Effectful type for GiftWrap.
type GiftWrapEff es = ( State AppState :> es
                      , Nostr :> es
                      , Util :> es
                      , Logging :> es   
                      , IOE :> es
                      )


-- Main effect handler
runGiftWrap :: GiftWrapEff es => Eff (GiftWrap : es) a -> Eff es a
runGiftWrap = interpret $ \_ -> \case
  HandleGiftWrapEvent event' -> do
    st <- get @AppState
    case keyPair st of
      Just kp -> do
        unwrappedEvent <- unwrapGiftWrap event' kp
        case unwrappedEvent of
          Just sealedEvent -> handleSealedEvent sealedEvent event' kp
          Nothing -> logInfo "Failed to unwrap gift wrap"
      Nothing -> logWarning "No key pair available to decrypt gift wrap"


-- Helper functions

-- | Handle a sealed event.
handleSealedEvent :: GiftWrapEff es => Event -> Event -> KeyPair -> Eff es ()
handleSealedEvent sealedEvent originalEvent kp
  | not (validateEvent sealedEvent) =
      logWarning $ "Invalid sealed event: " <> (byteStringToHex $ getEventId (eventId originalEvent))
  | kind sealedEvent /= Seal =
      logInfo "Unwrapped event is not a Seal"
  | otherwise = do
      unwrappedRumor <- unwrapSeal sealedEvent kp
      case unwrappedRumor of
        Just decryptedRumor -> processDecryptedRumor decryptedRumor sealedEvent originalEvent kp
        Nothing -> logInfo "Failed to decrypt sealed event"


-- | Process a decrypted rumor.
processDecryptedRumor :: GiftWrapEff es => Rumor -> Event -> Event -> KeyPair -> Eff es ()
processDecryptedRumor decryptedRumor sealedEvent originalEvent kp
  | not (pubKey sealedEvent == rumorPubKey decryptedRumor) =
      logWarning $ "Rumor pubkey does not match sealed event pubkey: " <> (byteStringToHex $ getEventId (eventId originalEvent))
  | otherwise = do
      let chatKey = if rumorPubKey decryptedRumor == keyPairToPubKeyXO kp
            then sort $ getAllPTags (rumorTags decryptedRumor)
            else filter (/= keyPairToPubKeyXO kp) $ rumorPubKey decryptedRumor : sort (getAllPTags (rumorTags decryptedRumor))
      let senderPubKey = rumorPubKey decryptedRumor
      ct <- getCurrentTime
      updateChats chatKey $ createChatMessage originalEvent decryptedRumor senderPubKey ct


-- | Update chats.
updateChats :: State AppState :> es => [PubKeyXO] -> ChatMessage -> Eff es ()
updateChats chatKeys chatMsg = do
  let chatKey = case chatKeys of
        (k:_) -> k
        [] -> error "Empty chat key list"
  modify $ \s -> s { chats = mergeMessageIntoChats chatKey chatMsg (chats s) }


-- | Get all p tags from the rumor tags
getAllPTags :: [Tag] -> [PubKeyXO]
getAllPTags = mapMaybe extractPubKey
  where
    extractPubKey (PTag pk _ _) = Just pk
    extractPubKey _ = Nothing


-- | Create a chat message.
createChatMessage :: Event -> Rumor -> PubKeyXO -> Int -> ChatMessage
createChatMessage originalEvent decryptedRumor senderPubKey currentTimestamp =
  ChatMessage
    { chatMessageId = eventId originalEvent
    , chatMessage = rumorContent decryptedRumor
    , author = senderPubKey
    , chatMessageCreatedAt = rumorCreatedAt decryptedRumor
    , timestamp = formatDateTime English currentTimestamp (rumorCreatedAt decryptedRumor)
    }


-- | Merge a new chat message into the existing chat map
mergeMessageIntoChats :: PubKeyXO -> ChatMessage -> Map.Map PubKeyXO [ChatMessage] -> Map.Map PubKeyXO [ChatMessage]
mergeMessageIntoChats chatKey chatMsg = Map.alter (addMessageIfUnique chatMsg) chatKey


-- | Add a message to a chat thread only if it doesn't already exist
-- Messages are sorted by creation time, newest last
addMessageIfUnique :: ChatMessage -> Maybe [ChatMessage] -> Maybe [ChatMessage]
addMessageIfUnique chatMsg = \case
    Nothing -> Just [chatMsg]
    Just msgs ->
        if any (\msg -> chatMessageId msg == chatMessageId chatMsg) msgs
            then Just msgs
            else Just $ sortBy (comparing chatMessageCreatedAt) (chatMsg : msgs)
