{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Widgets.ViewProfile where

import           Control.Concurrent.STM.TChan
import           Control.Lens
import           Control.Monad.STM                    (atomically)
import           Crypto.Schnorr
import           Data.DateTime
import           Data.Default
import qualified Data.Map                             as Map
import           Data.Text
import           Monomer

import           Helpers
import           NostrFunctions
import           NostrTypes

data ViewProfileModel =  ViewProfileModel
  { _xo              :: Text
  , _name            :: Text
  , _about           :: Text
  , _pictureUrl      :: Text
  , _nip05Identifier :: Text
  , _following       :: Map.Map Keys [Profile]
  , _posts           :: [ReceivedEvent]
  } deriving (Eq, Show)

instance Default ViewProfileModel where
  def = ViewProfileModel "" "" "" "" "" Map.empty []

data ProfileEvent
  = Follow
  | Unfollow
  deriving (Eq, Show)

makeLenses 'ViewProfileModel

handleProfileEvent
  :: TChan ServerRequest
  -> Keys
  -> WidgetEnv ViewProfileModel ProfileEvent
  -> WidgetNode ViewProfileModel ProfileEvent
  -> ViewProfileModel
  -> ProfileEvent
  -> [EventResponse ViewProfileModel ProfileEvent sp ep]
handleProfileEvent chan ks env node model evt = case evt of
  Follow ->
    [ Producer $ follow chan ks model ]
  Unfollow ->
    [ Producer $ unfollow chan ks model ]

viewProfileWidget
  :: (WidgetModel sp, WidgetEvent ep)
  => TChan ServerRequest
  -> Keys
  -> ALens' sp ViewProfileModel
  -> WidgetNode sp ep
viewProfileWidget chan keys field = composite "ViewProfileWidget" field viewProfile (handleProfileEvent chan keys)

follow :: TChan ServerRequest -> Keys -> ViewProfileModel -> (ProfileEvent -> IO ()) -> IO ()
follow chan (Keys kp xo _ _) model sendMsg = do
  now <- getCurrentTime
  return ()
  -- let raw = setMetadata name about picture nip05 xo now
  -- atomically $ writeTChan chan $ SendEvent $ signEvent raw kp xo
  -- where
  --   is = model ^. inputs
  --   name = strip $ is ^. nameInput
  --   about = strip $ is ^. aboutInput
  --   picture = strip $ is ^. pictureUrlInput
  --   nip05 = strip $ is ^. nip05IdentifierInput

unfollow :: TChan ServerRequest -> Keys -> ViewProfileModel -> (ProfileEvent -> IO ()) -> IO ()
unfollow chan (Keys kp xo _ _) model sendMsg = do
  now <- getCurrentTime
  return ()
  -- let raw = setMetadata name about picture nip05 xo now
  -- atomically $ writeTChan chan $ SendEvent $ signEvent raw kp xo
  -- where
  --   is = model ^. inputs
  --   name = strip $ is ^. nameInput
  --   about = strip $ is ^. aboutInput
  --   picture = strip $ is ^. pictureUrlInput
  --   nip05 = strip $ is ^. nip05IdentifierInput

viewProfile :: WidgetEnv ViewProfileModel ProfileEvent -> ViewProfileModel -> WidgetNode ViewProfileModel ProfileEvent
viewProfile wenv model =
  vstack
    [ label $ model ^. name
    , spacer
    , (label $ model ^. xo) `styleBasic` [ textSize 10 ]
    , spacer
    , hstack
        [ label "Name: "
        , filler
        ]
    , spacer
    , hstack
        [ label "About"
        , filler
        , label $ model ^. about
        ]
    , spacer
    , hstack
        [ label "Picture URL"
        , filler
        , label $ model ^. pictureUrl
        ]
    , spacer
    , hstack
        [ label "NIP-05 Identifier"
        , filler
        , label $ model ^. nip05Identifier
        ]
    , spacer
    , button "Follow" Follow
    ] `styleBasic` [padding 10]
