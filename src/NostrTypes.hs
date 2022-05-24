{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module NostrTypes where

import           Control.Monad          (mzero, (<=<))
import           Crypto.Schnorr         (KeyPair, Msg, SchnorrSig, XOnlyPubKey,
                                         verifyMsgSchnorr)
import qualified Crypto.Schnorr         as Schnorr
import           Data.Aeson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import           Data.ByteString.Lazy   (toStrict)
import           Data.Default
import           Data.Text              (Text, pack, unpack)
import           Data.DateTime
import qualified Data.Vector            as V
import           Foreign.C.Types        (CTime (..))
import           GHC.Exts               (fromList)
import           Network.Socket         (PortNumber)

data Relay =
  Relay
  { host    :: String
  , port    :: PortNumber
  , secure  :: Bool
  , readable  :: Bool
  , writable  :: Bool
  , connected :: Bool
  }
  deriving (Eq, Show)

defaultPool :: [Relay]
defaultPool =
  [
  --   Relay
  --   { host = "nostr-pub.wellorder.net"
  --   , port = 443
  --   , secure = True
  --   , readable = True
  --   , writable = True
  --   , connected = False
  --   }
  -- ,
    Relay
    { host = "localhost"
    , port = 2700
    , secure = False
    , readable = True
    , writable = True
    , connected = False
    }
  ]

type RelayURL = Text

newtype EventId =
  EventId
  { getEventId :: ByteString
  }
  deriving (Eq)

data ServerRequest
  = SendEvent Event
  | RequestRelay Text [EventFilter]
  | Close Text
  | Disconnect Relay
  deriving (Eq, Show)

data ServerResponse = ServerResponse Text Event
  deriving (Eq, Show)

-- | Keys
-- | The bool value declares if the keys are active
-- | The text value gives the profile name
data Keys = Keys KeyPair XOnlyPubKey Bool (Maybe Text)
  deriving (Eq, Show)

instance Ord Keys where
  compare (Keys a _ _ _) (Keys b _ _ _) =
    compare (Schnorr.getKeyPair a) (Schnorr.getKeyPair b)

instance Ord XOnlyPubKey where
  compare a b =
    compare (Schnorr.getXOnlyPubKey a) (Schnorr.getXOnlyPubKey b)

instance FromJSON Keys where
  parseJSON = withArray "Keys" $ \arr -> do
    kp <- parseJSON $ arr V.! 0
    xo <- parseJSON $ arr V.! 1
    a  <- parseJSON $ arr V.! 2
    n  <- parseJSON $ arr V.! 3
    return $ Keys kp xo a n

instance ToJSON Keys where
  toJSON (Keys kp xo a n) =
    Array $ fromList
      [ toJSON kp
      , toJSON xo
      , toJSON a
      , toJSON n
      ]

data Post =
  Post
  { postId :: EventId
  , author :: Text
  , postContent :: Text
  , posted :: DateTime
  }
  deriving (Eq, Show)

instance ToJSON ServerRequest where
  toJSON sr = case sr of
    SendEvent e -> Array $ fromList
       [ String $ pack "EVENT"
       , toJSON e
       ]
    RequestRelay s efs -> Array $ fromList
      ([ String $ pack "REQ"
      , String $ s
       ] ++ map (\ef -> toJSON ef) efs)
    Close subId -> Array $ fromList
       [ String $ pack "CLOSE"
       , String subId
       ]
    Disconnect r -> String $ pack "Bye!"

instance Show EventId where
  showsPrec _ = shows . B16.encodeBase16 . getEventId

instance ToJSON KeyPair where
  toJSON e = String $ pack $ Schnorr.exportKeyPair e

instance ToJSON EventId where
  toJSON e = String $ pack $ exportEventId e

instance ToJSON SchnorrSig where
  toJSON s = String $ pack $ Schnorr.exportSchnorrSig s

instance ToJSON XOnlyPubKey where
  toJSON x = String $ pack $ Schnorr.exportXOnlyPubKey x

instance FromJSON KeyPair where
  parseJSON = withText "KeyPair" $ \k -> do
    case (textToByteStringType k Schnorr.keypair) of
      Just k' -> return k'
      _     -> fail "invalid key pair"

instance FromJSON EventId where
  parseJSON = withText "EventId" $ \i -> do
    case eventId' i of
      Just e -> return e
      _    -> fail "invalid event id"

instance FromJSON SchnorrSig where
  parseJSON = withText "SchnorrSig" $ \s -> do
    case (textToByteStringType s Schnorr.schnorrSig) of
      Just s' -> return s'
      _     -> fail "invalid schnorr sig"

instance FromJSON ServerResponse where
  parseJSON = withArray "ServerResponse Event" $ \arr -> do
    t <- parseJSON $ arr V.! 0
    s <- parseJSON $ arr V.! 1
    e <- parseJSON $ arr V.! 2
    case t of
      String "EVENT" -> return $ ServerResponse s e
      _ -> fail "Invalid ServerResponse did not have EVENT"

instance FromJSON XOnlyPubKey where
  parseJSON = withText "XOnlyPubKey" $ \p -> do
    case (textToByteStringType p Schnorr.xOnlyPubKey) of
      Just e -> return e
      _    -> fail "invalid XOnlyPubKey"

data Event =
  Event
  { eventId    :: EventId
  , pubKey     :: XOnlyPubKey
  , created_at :: DateTime
  , kind       :: Int
  , tags       :: [Tag]
  , content    :: Text
  , sig        :: SchnorrSig
  }
  deriving (Eq, Show)

instance ToJSON Event where
  toJSON Event {..} = object
     [ "id"         .= exportEventId eventId
     , "pubkey"     .= Schnorr.exportXOnlyPubKey pubKey
     , "created_at" .= toSeconds created_at
     , "kind"       .= kind
     , "tags"       .= tags
     , "content"    .= content
     , "sig"        .= Schnorr.exportSchnorrSig sig
     ]

instance FromJSON Event where
  parseJSON = withObject "event data" $ \e -> Event
    <$> e .: "id"
    <*> e .: "pubkey"
    <*> (fromSeconds <$> e .: "created_at")
    <*> e .: "kind"
    <*> e .: "tags"
    <*> e .: "content"
    <*> e .: "sig"

data RawEvent =
  RawEvent
  { pubKey'     :: XOnlyPubKey
  , created_at' :: DateTime
  , kind'       :: Int
  , tags'       :: [Tag]
  , content'    :: Text
  }
  deriving (Eq, Show)

data Profile = Profile XOnlyPubKey RelayURL ProfileData
  deriving (Eq, Show)

instance FromJSON Profile where
  parseJSON (Array v)
    | V.length v == 3 =
      case v V.! 0 of
        String "p" ->
          Profile <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2) <*> parseJSON ""
        _ -> fail "Unknown profile"
    | V.length v == 4 && v V.! 0 == String "p" =
        Profile <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2) <*> parseJSON (v V.! 3)
    | otherwise = fail "Invalid profile"
  parseJSON _ = fail "Cannot parse profile"

instance ToJSON Profile where
  toJSON (Profile xo relayURL pd) =
    Array $ fromList
      [ String "p"
      , String $ pack $ Schnorr.exportXOnlyPubKey xo
      , String relayURL
      , String $ pdName pd
      ]

type ReceivedEvent = (Event, [Relay])

data ProfileData =
  ProfileData
  { pdName       :: Text
  , pdAbout      :: Text
  , pdPictureUrl :: Text
  , pdNip05      :: Text
  }
  deriving (Eq, Show)

instance Default ProfileData where
  def = ProfileData "" "" "" ""

instance FromJSON ProfileData where
  parseJSON = withObject "profile data" $ \e -> ProfileData
    <$> e .: "name"
    <*> e .: "about"
    <*> e .: "picture"
    <*> e .: "nip05"

data ForeignXOnlyPubKey
  = ValidXOnlyPubKey XOnlyPubKey
  | InvalidXOnlyPubKey
  deriving (Eq, Show)

instance FromJSON ForeignXOnlyPubKey where
  parseJSON = withText "foreign XOnlyPubKey" $ \t -> do
    case textToByteStringType t Schnorr.xOnlyPubKey of
      Just xo ->
        return $ ValidXOnlyPubKey xo
      Nothing ->
        return InvalidXOnlyPubKey

instance ToJSON ForeignXOnlyPubKey where
  toJSON (ValidXOnlyPubKey xo) = toJSON xo
  toJSON _ = Null

data Tag
  = ETag EventId (Maybe RelayURL)
  | PTag ForeignXOnlyPubKey (Maybe RelayURL) (Maybe Text)
  | NonceTag
  | UnknownTag
  deriving (Eq, Show)

data EventFilter
  = AllProfilesFilter (Maybe DateTime)
  | OwnEventsFilter XOnlyPubKey DateTime
  | MentionsFilter XOnlyPubKey DateTime
  | FollowersFilter [Profile] DateTime
  | ProfileFollowers XOnlyPubKey
  deriving (Eq, Show)

instance FromJSON Tag where
  parseJSON (Array v)
    | V.length v > 0 =
        case v V.! 0 of
          String "e" ->
            ETag <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2)
          String "p" ->
            PTag <$> parseJSON (v V.! 1) <*> parseJSON (v V.! 2) <*> parseJSON (v V.! 3)
          _ ->
            return UnknownTag
    | otherwise = return UnknownTag
  parseJSON _ = return UnknownTag

instance ToJSON Tag where
  toJSON (ETag eventId relayURL) =
    Array $ fromList
      [ String "e"
      , String $ pack $ exportEventId eventId
      , case relayURL of
          Just relayURL' ->
            String relayURL'
          Nothing ->
            Null
      ]
  toJSON (PTag xo relayURL name) =
    Array $ fromList
      [ String "p"
      , case xo of
          ValidXOnlyPubKey xo' ->
            toJSON xo'
          InvalidXOnlyPubKey ->
            Null
      , toJSON relayURL
      , toJSON name
      ]
  toJSON _ = -- @todo implement nonce tag
    Array $ fromList []

instance ToJSON EventFilter where
  toJSON (AllProfilesFilter Nothing) =
    object $ fromList
      [ ( "kinds", Array $ fromList $ [ Number 0 ]) ]
  toJSON (AllProfilesFilter (Just d)) =
    object $ fromList
      [ ( "kinds", Array $ fromList $ [ Number 0 ])
      , ( "since", Number $ fromIntegral $ toSeconds d)
      ]
  toJSON (OwnEventsFilter xo d) =
    object $ fromList
      [ ( "kinds", Array $ fromList $ [ Number 1, Number 3, Number 4 ] )
      , ( "authors", Array $ fromList $ [ String $ pack $ Schnorr.exportXOnlyPubKey xo ])
      , ( "since", Number $ fromIntegral $ toSeconds d)
      ]
  toJSON (MentionsFilter xo  d) =
    object $ fromList
      [ ( "kinds", Array $ fromList $ [ Number 1, Number 4 ])
      , ( "#p", Array $ fromList $ [ String $ pack $ Schnorr.exportXOnlyPubKey xo ])
      , ( "since", Number $ fromIntegral $ toSeconds d)
      ]
  toJSON (FollowersFilter ps d) =
    object $ fromList
      [ ( "kinds", Array $ fromList $ [ Number 1, Number 3 ] )
      , ( "authors", Array $ fromList $ map String $ map (pack . Schnorr.exportXOnlyPubKey) keys)
      , ( "since", Number $ fromIntegral $ toSeconds d)
      ]
      where
        keys = map (\(Profile xo _ _) -> xo) ps
  toJSON (ProfileFollowers xo) =
    object $ fromList
      [ ( "kinds", Array $ fromList $ [ Number 3 ] )
      , ( "authors", Array $ fromList [ String $ pack $ Schnorr.exportXOnlyPubKey xo ] )
      , ( "limit", Number $ fromIntegral 1 )
      ]


textToByteStringType :: Text -> (ByteString -> Maybe a) -> Maybe a
textToByteStringType t f = case Schnorr.decodeHex t of
  Just bs -> f bs
  Nothing -> Nothing

eventId' :: Text -> Maybe EventId
eventId' t = do
  bs <- Schnorr.decodeHex t
  case BS.length bs of
    32 -> Just $ EventId bs
    _  -> Nothing

exportEventId :: EventId -> String
exportEventId i = unpack . B16.encodeBase16 $ getEventId i
