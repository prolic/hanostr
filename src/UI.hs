{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module UI where

import Data.Aeson (decode, encode)
import Data.ByteString.Lazy qualified as BSL
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (pack, unpack)
import Data.Text.Encoding qualified as TE
import Effectful
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (get, modify)
import Effectful.TH
import QtQuick
import Graphics.QML hiding (fireSignal, runEngineLoop)
import Text.Read (readMaybe)

import Logging
import Nostr.Bech32
import Nostr.Event
import Nostr.Publisher
import Nostr.Keys (PubKeyXO, keyPairToPubKeyXO)
import Nostr.Types (EventId(..), Profile(..), getUri)
import Nostr.Util
import Presentation.KeyMgmtUI qualified as KeyMgmtUI
import Presentation.RelayMgmtUI qualified as RelayMgmtUI
import Futr hiding (Comment, QuoteRepost, Repost)
import Store.Profile (getProfile)
import TimeFormatter
import Types

-- | Key Management Effect for creating QML UI.
data UI :: Effect where
  CreateUI :: SignalKey (IO ()) -> UI m (ObjRef ())


-- | Dispatch type for UI effect.
type instance DispatchOf UI = Dynamic


makeEffect ''UI


-- | Run the UI effect.
runUI :: (FutrEff es, Futr :> es) => Eff (UI : es) a -> Eff es a
runUI = interpret $ \_ -> \case
  CreateUI changeKey' -> withEffToIO (ConcUnlift Persistent Unlimited) $ \runE -> do
    keyMgmtObj <- runE $ KeyMgmtUI.createUI changeKey'
    relayMgmtObj <- runE $ RelayMgmtUI.createUI changeKey'

    profileClass <- newClass [
        defPropertySigRO' "name" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ name profile,

        defPropertySigRO' "displayName" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ displayName profile,

        defPropertySigRO' "about" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ about profile,

        defPropertySigRO' "picture" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ picture profile,

        defPropertySigRO' "nip05" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ nip05 profile,

        defPropertySigRO' "banner" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let pk = fromMaybe (error "No pubkey for current profile") $ currentProfile st
          (profile, _) <- runE $ getProfile pk
          return $ banner profile,

        defPropertySigRO' "isFollow" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          let currentPubKey = keyPairToPubKeyXO <$> keyPair st
          let profilePubKey = currentProfile st
          case (currentPubKey, profilePubKey) of
            (Just userPK, Just profilePK) -> do
              let userFollows = Map.findWithDefault [] userPK (follows st)
              return $ any (\follow -> pubkey follow == profilePK) userFollows
            _ -> return False
        ]

    let followProp name' accessor = defPropertySigRO' name' changeKey' $ \obj -> do
          let pubKeyXO = fromObjRef obj :: PubKeyXO
          st <- runE $ get @AppState
          let followData = case keyPairToPubKeyXO <$> keyPair st of
                Just upk | upk == pubKeyXO ->
                  Just Follow { pubkey = pubKeyXO, followRelay = Nothing, petName = Nothing }
                Just upk ->
                  Map.lookup upk (follows st) >>= find (\f -> pubkey f == pubKeyXO)
                Nothing -> Nothing
          accessor st followData

    followClass <- newClass [
        followProp "pubkey" $ \_ -> return . maybe "" (pubKeyXOToBech32 . pubkey),
        followProp "relay" $ \_ -> return . maybe "" (\f -> maybe "" getUri (followRelay f)),
        followProp "petname" $ \_ -> return . maybe "" (fromMaybe "" . petName),
        followProp "displayName" $ \_ -> maybe (return "") (\follow -> do
            (profile, _) <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (displayName profile)),
        followProp "name" $ \_ -> maybe (return "") (\follow -> do
            (profile, _) <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (name profile)),
        followProp "picture" $ \_ -> maybe (return "") (\follow -> do
            (profile, _) <- runE $ getProfile (pubkey follow)
            return $ fromMaybe "" (picture profile))
      ]

    followPool <- newFactoryPool (newObject followClass)

    let getPostProperty obj extractor = do
          st <- runE $ get @AppState
          let eid = fromObjRef obj :: EventId
          case currentContact st of
            (Nothing, _) -> return Nothing
            (Just recipient, _) -> do
              let notes = Map.findWithDefault [] recipient (posts st)
              case find (\msg -> postId msg == eid) notes of
                Nothing -> return Nothing
                Just msg -> extractor msg

    postClass <- newClass [
            defPropertySigRO' "id" changeKey' $ \obj -> do
              let eid = fromObjRef obj :: EventId
              return $ pack $ show eid,

            defPropertySigRO' "postType" changeKey' $ \obj -> do
              getPostProperty obj $ \msg ->
                return $ Just $ pack $ case postType msg of
                  ShortTextNote -> "short_text_note"
                  Repost _ -> "repost"
                  QuoteRepost _ -> "quote_repost"
                  Comment _ _ _ _ -> "comment",

            defPropertySigRO' "referencedEventId" changeKey' $ \obj -> do
              getPostProperty obj $ \msg ->
                return $ Just $ pack $ case postType msg of
                  Repost ref -> show ref
                  QuoteRepost ref -> show ref
                  Comment{rootScope=ref} -> show ref
                  _ -> "",

            defPropertySigRO' "content" changeKey' $ \obj -> do
              getPostProperty obj $ \msg ->
                runE $ getPostContent msg,

            defPropertySigRO' "timestamp" changeKey' $ \obj -> do
              getPostProperty obj $ \msg -> do
                ts <- runE $ getPostCreatedAt msg
                case ts of
                  Just ts' -> do
                    now <- runE getCurrentTime
                    return $ Just $ formatDateTime English now ts'
                  Nothing -> return Nothing,

            defPropertySigRO' "referencedContent" changeKey' $ \obj -> do
              getPostProperty obj $ \msg -> case postType msg of
                Repost ref -> do
                  content <- runE $ getReferencedContent ref
                  return $ content
                QuoteRepost ref -> do
                  content <- runE $ getReferencedContent ref
                  return $ content
                _ -> return Nothing,

            defPropertySigRO' "referencedAuthorPubkey" changeKey' $ \obj -> do
              getPostProperty obj $ \msg -> case postType msg of
                Repost ref -> do
                  authorMaybe <- runE $ getReferencedAuthor ref
                  return $ fmap pubKeyXOToBech32 authorMaybe
                QuoteRepost ref -> do
                  authorMaybe <- runE $ getReferencedAuthor ref
                  return $ fmap pubKeyXOToBech32 authorMaybe
                Comment{rootScope=ref} -> do
                  authorMaybe <- runE $ getReferencedAuthor ref
                  return $ fmap pubKeyXOToBech32 authorMaybe
                _ -> return Nothing,

            defPropertySigRO' "referencedAuthorName" changeKey' $ \obj -> do
              getPostProperty obj $ \msg -> case postType msg of
                Repost ref -> runE $ getReferencedAuthorName ref
                QuoteRepost ref -> runE $ getReferencedAuthorName ref
                Comment{rootScope=ref} -> runE $ getReferencedAuthorName ref
                _ -> return Nothing,

            defPropertySigRO' "referencedAuthorPicture" changeKey' $ \obj -> do
              getPostProperty obj $ \msg -> case postType msg of
                Repost ref -> runE $ getReferencedAuthorPicture ref
                QuoteRepost ref -> runE $ getReferencedAuthorPicture ref
                Comment{rootScope=ref} -> runE $ getReferencedAuthorPicture ref
                _ -> return Nothing,

            defPropertySigRO' "referencedCreatedAt" changeKey' $ \obj -> do
              let getFormattedTime ref = do
                    tsM <- runE $ getReferencedCreatedAt ref
                    case tsM of
                      Just ts -> do
                        now <- runE getCurrentTime
                        return $ Just $ formatDateTime English now ts
                      Nothing -> return Nothing
              getPostProperty obj $ \msg -> case postType msg of
                Repost ref -> getFormattedTime ref
                QuoteRepost ref -> getFormattedTime ref
                Comment{rootScope=ref} -> getFormattedTime ref
                _ -> return Nothing
          ]

    postsPool <- newFactoryPool (newObject postClass)

    chatClass <- newClass [
        defPropertySigRO' "id" changeKey' $ \obj -> do
          let eid = fromObjRef obj :: EventId
          return $ pack $ show eid,

        defPropertySigRO' "content" changeKey' $ \obj -> do
          st <- runE $ get @AppState
          let eid = fromObjRef obj :: EventId
          let currentRecipient = currentContact st
          case currentRecipient of
            (Just recipient, _) -> do
              let chatMessages = Map.findWithDefault [] recipient (chats st)
              case find (\msg -> chatMessageId msg == eid) chatMessages of
                Just msg -> return $ chatMessage msg
                Nothing -> return ""
            _ -> return "",

        defPropertySigRO' "isOwnMessage" changeKey' $ \obj -> do
          st <- runE $ get @AppState
          let eid = fromObjRef obj :: EventId
          let pk = keyPairToPubKeyXO <$> keyPair st
          let currentRecipient = currentContact st
          case (pk, currentRecipient) of
            (Just userPk, (Just recipient, _)) -> do
              let chatMessages = Map.findWithDefault [] recipient (chats st)
              case find (\msg -> chatMessageId msg == eid) chatMessages of
                Just msg -> return $ author msg == userPk
                Nothing -> return False
            _ -> return False,

        defPropertySigRO' "timestamp" changeKey' $ \obj -> do
          st <- runE $ get @AppState
          let eid = fromObjRef obj :: EventId
          case currentContact st of
            (Just recipient, _) -> do
              let chatMessages = Map.findWithDefault [] recipient (chats st)
              case find (\msg -> chatMessageId msg == eid) chatMessages of
                Just msg -> return $ timestamp msg
                Nothing -> return ""
            _ -> return ""
      ]

    chatPool <- newFactoryPool (newObject chatClass)

    rootClass <- newClass [
        defPropertyConst' "ctxKeyMgmt" (\_ -> return keyMgmtObj),

        defPropertyConst' "ctxRelayMgmt" (\_ -> return relayMgmtObj),

        defPropertyConst' "currentProfile" (\_ -> do
          profileObj <- newObject profileClass ()
          runE $ modify @QtQuickState $ \st -> st { uiRefs = (uiRefs st) { profileObjRef = Just profileObj } }
          return profileObj
        ),

        defPropertySigRW' "currentScreen" changeKey'
            (\_ -> do
              st <- runE $ get @AppState
              return $ pack $ show $ currentScreen st)
            (\obj newScreen -> do
                case readMaybe (unpack newScreen) :: Maybe AppScreen of
                    Just s -> do
                        runE $ do
                          modify $ \st -> st { currentScreen = s }
                          fireSignal obj
                    Nothing -> return ()),

        defPropertySigRO' "mynpub" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          return $ case keyPair st of
            Just kp -> pubKeyXOToBech32 $ keyPairToPubKeyXO kp
            Nothing -> "",

        defPropertySigRO' "mypicture" changeKey' $ \_ -> do
          st <- runE $ get @AppState
          case keyPair st of
            Just kp -> do
              (profile', _) <- runE $ getProfile $ keyPairToPubKeyXO kp
              return $ fromMaybe "" $ picture profile'
            Nothing -> return "",

        defSignal "loginStatusChanged" (Proxy :: Proxy LoginStatusChanged),

        defMethod' "login" $ \obj input -> runE $ login obj input,

        defMethod' "logout" $ \obj -> runE $ logout obj,

        defMethod' "search" $ \obj input -> runE $ do
          res <- search obj input
          return $ TE.decodeUtf8 $ BSL.toStrict $ encode res,

        defMethod' "setCurrentProfile" $ \_ npub' -> runE $ setCurrentProfile npub',

        defMethod' "saveProfile" $ \_ input -> do
          let profile = maybe (error "Invalid profile JSON") id $ decode (BSL.fromStrict $ TE.encodeUtf8 input) :: Profile
          n <- runE getCurrentTime
          kp <- runE getKeyPair
          let unsigned = createMetadata profile (keyPairToPubKeyXO kp) n
          signedMaybe <- signEvent unsigned kp
          case signedMaybe of
            Just signed -> do
              runE $ broadcast signed
              runE $ logInfo "Profile successfully saved and sent to relay pool"
            Nothing -> runE $ logWarning "Failed to sign profile update event",

        defPropertySigRO' "followList" changeKey' $ \obj -> do
          runE $ modify $ \s -> s { uiRefs = (uiRefs s) { followsObjRef = Just obj } }
          st <- runE $ get @AppState
          let maybeUserPubKey = keyPairToPubKeyXO <$> keyPair st
          case maybeUserPubKey of
            Just userPubKey -> do
              let userFollows = Map.findWithDefault [] userPubKey (follows st)
              let selfFollow = Follow { pubkey = userPubKey, followRelay = Nothing, petName = Nothing }
              objs <- mapM (getPoolObject followPool) (map pubkey (selfFollow : userFollows))
              return objs
            Nothing -> return [],

        defPropertySigRO' "posts" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { postsObjRef = Just obj } }
          st <- runE $ get @AppState
          case currentContact st of
            (Just recipient, _) -> do
              let notes = Map.findWithDefault [] recipient (posts st)
              objs <- mapM (getPoolObject postsPool) (map postId notes)
              return objs
            _ -> do return [],

        defPropertySigRO' "privateMessages" changeKey' $ \obj -> do
          runE $ modify @QtQuickState $ \s -> s { uiRefs = (uiRefs s) { privateMessagesObjRef = Just obj } }
          st <- runE $ get @AppState
          case currentContact st of
            (Just recipient, _) -> do
              let chatMessages = Map.findWithDefault [] recipient (chats st)
              objs <- mapM (getPoolObject chatPool) (map chatMessageId chatMessages)
              return objs
            _ -> do return [],

        defMethod' "follow" $ \_ npubText -> runE $ followProfile npubText,

        defMethod' "unfollow" $ \_ npubText -> runE $ unfollowProfile npubText,

        defMethod' "openChat" $ \_ npubText -> runE $ do
            let pubKeyXO = maybe (error "Invalid bech32 public key") id $ bech32ToPubKeyXO npubText
            openChat pubKeyXO,

        defMethod' "sendPrivateMessage" $ \_ input -> runE $ sendPrivateMessage input, -- NIP-17 private direct message

        defMethod' "sendShortTextNote" $ \_ input -> runE $ sendShortTextNote input, -- NIP-01 short text note

        defMethod' "repost" $ \_ eid -> runE $ do -- NIP-18 repost
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          repost eid',

        defMethod' "quoteRepost" $ \_ eid quote -> runE $ do -- NIP-18 quote repost
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          quoteRepost eid' quote,

        defMethod' "comment" $ \_ eid input -> runE $ do -- NIP-22 comment
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          comment eid' input,

        defMethod' "deleteEvent" $ \_ eid input -> runE $ do -- NIP-09 delete post
          let unquoted = read (unpack eid) :: String
          let eid' = read unquoted :: EventId
          deleteEvent eid' input
      ]

    rootObj <- newObject rootClass ()

    return rootObj
