{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Effectful.Concurrent.STM (TChan, TQueue)
import Lmdb.Types (Database, Environment, Mode(..))
import GHC.Generics (Generic)
import Nostr.Keys (KeyPair, PubKeyXO)
import Nostr.Types (Event, EventId, Filter, Profile, Relay, RelayURI, Request, SubscriptionId)


-- | Status of a publish operation
data PublishStatus
    = Publishing
    | WaitingForConfirmation
    | Success
    | Failure Text
    deriving (Eq, Show)


-- | Subscription events
data SubscriptionEvent
    = EventAppeared Event
    | SubscriptionEose
    | SubscriptionClosed Text


-- | State for RelayPool handling.
data RelayPoolState = RelayPoolState
    { activeConnections :: Map RelayURI RelayData
    , publishStatus :: Map EventId (Map RelayURI PublishStatus)
    , generalRelays :: Map PubKeyXO ([Relay], Int)
    , dmRelays :: Map PubKeyXO ([Relay], Int)
    }


-- | Subscription details.
data SubscriptionDetails = SubscriptionDetails
    { subscriptionId :: SubscriptionId
    , subscriptionFilters :: [Filter]
    , responseQueue :: TQueue SubscriptionEvent
    , eventsProcessed :: Int
    , newestCreatedAt :: Int
    }


-- | Connection errors.
data ConnectionError 
    = ConnectionFailed Text
    | AuthenticationFailed Text
    | NetworkError Text
    | TimeoutError
    | InvalidRelayConfig
    | MaxRetriesReached
    | UserDisconnected
    deriving (Show, Eq)


-- | Relay connection state.
data ConnectionState = Connected | Disconnected | Connecting
  deriving (Show, Eq)


-- | Data for each relay.
data RelayData = RelayData
  { connectionState :: ConnectionState
  , requestChannel :: TChan Request
  , activeSubscriptions :: Map SubscriptionId SubscriptionDetails
  , notices        :: [Text]
  , lastError      :: Maybe ConnectionError
  , connectionAttempts :: Int
  , pendingRequests :: [Request]
  , pendingEvents :: [Event]
  , pendingAuthId :: Maybe EventId
  }


-- | Initial state for RelayPool.
initialRelayPoolState :: RelayPoolState
initialRelayPoolState = RelayPoolState
  { activeConnections = Map.empty
  , publishStatus = Map.empty
  , generalRelays = Map.empty
  , dmRelays = Map.empty
  }


-- | Application screens
data AppScreen
    = KeyMgmt
    | Home
    deriving (Eq, Read, Show)


-- | Chat message.
data ChatMessage = ChatMessage
  { chatMessageId :: EventId
  , chatMessage :: Text
  , author :: PubKeyXO
  , chatMessageCreatedAt :: Int
  , timestamp :: Text
  } deriving (Show)


-- | Type of post
data PostType
  = ShortTextNote
  | Repost EventId             -- kind 6, references original note
  | QuoteRepost EventId      -- kind 1 with q tag, includes quoted event and additional content
  | Comment {
      rootScope :: EventId,    -- root event being commented on
      rootKind :: Int,         -- kind of root event
      parentId :: EventId,     -- immediate parent (same as root for top-level comments)
      parentKind :: Int        -- kind of parent
    }
  deriving (Show, Eq)


-- | Simplified note reference that proxies most data through events map
data Post = Post
  { postId :: EventId          -- ID of this post
  , postType :: PostType       -- Type of post and its references
  , postCreatedAt :: Int       -- Creation timestamp
  } deriving (Show)


-- | Data type to store event with its relay sources
data EventWithRelays = EventWithRelays
    { event :: Event
    , relays :: Set RelayURI
    } deriving (Show, Generic, ToJSON, FromJSON)


-- | Application state.
data AppState = AppState
  { keyPair :: Maybe KeyPair
  , currentScreen :: AppScreen -- @todo remove maybe?
  , lmdbEnv :: Maybe (Environment ReadWrite)
  , eventDb :: Maybe (Database EventId EventWithRelays)
  , profileDb :: Maybe (Database PubKeyXO (Profile, Int))
  , posts :: Map PubKeyXO [Post]
  , chats :: Map PubKeyXO [ChatMessage]
  , follows :: Map PubKeyXO [Follow]
  , currentContact :: (Maybe PubKeyXO, Maybe SubscriptionId)
  , currentProfile :: Maybe PubKeyXO
  }


-- | Follow.
data Follow = Follow
  { pubkey :: PubKeyXO
  , followRelay :: Maybe Relay
  , petName :: Maybe Text
  } deriving (Show)


-- | Initial application state.
initialState :: AppState
initialState = AppState
  { keyPair = Nothing
  , currentScreen = KeyMgmt
  , lmdbEnv = Nothing
  , eventDb = Nothing
  , profileDb = Nothing
  , posts = Map.empty
  , chats = Map.empty
  , follows = Map.empty
  , currentContact = (Nothing, Nothing)
  , currentProfile = Nothing
  }
