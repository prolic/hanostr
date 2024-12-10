{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module QtQuick where

import Control.Monad (forever, forM_, void, when)
import Effectful
import Effectful.Concurrent
import Effectful.Concurrent.Async (async)
import Effectful.Concurrent.STM (TQueue, atomically, flushTQueue, newTQueueIO, readTQueue, writeTQueue)
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.State.Static.Shared (State, get, gets, put)
import Effectful.TH
import Graphics.QML qualified as QML

import Logging
import Types (AppState(..))


-- | Effectful QML state.
data QtQuickState = QtQuickState
  { signalKey :: Maybe (QML.SignalKey (IO ()))
  , rootObjRef :: Maybe (QML.ObjRef ())
  , uiRefs :: UIReferences
  , queue :: Maybe (TQueue UIUpdates)
  }


-- | UI object references grouped together
data UIReferences = UIReferences
  { profileObjRef :: Maybe (QML.ObjRef ())
  , followsObjRef :: Maybe (QML.ObjRef ())
  , postsObjRef :: Maybe (QML.ObjRef ())
  , privateMessagesObjRef :: Maybe (QML.ObjRef ())
  , dmRelaysObjRef :: Maybe (QML.ObjRef ())
  , generalRelaysObjRef :: Maybe (QML.ObjRef ())
  , tempRelaysObjRef :: Maybe (QML.ObjRef ())
  }



-- | UI updates
data UIUpdates = UIUpdates
  { profilesChanged :: Bool
  , followsChanged :: Bool
  , postsChanged :: Bool
  , privateMessagesChanged :: Bool
  , dmRelaysChanged :: Bool
  , generalRelaysChanged :: Bool
  , tempRelaysChanged :: Bool
  , publishStatusChanged :: Bool
  , noticesChanged :: Bool
  } deriving (Eq, Show)


instance Semigroup UIUpdates where
  a <> b = UIUpdates
    { profilesChanged = profilesChanged a || profilesChanged b
    , followsChanged = followsChanged a || followsChanged b
    , postsChanged = postsChanged a || postsChanged b
    , privateMessagesChanged = privateMessagesChanged a || privateMessagesChanged b
    , dmRelaysChanged = dmRelaysChanged a || dmRelaysChanged b
    , generalRelaysChanged = generalRelaysChanged a || generalRelaysChanged b
    , tempRelaysChanged = tempRelaysChanged a || tempRelaysChanged b
    , publishStatusChanged = publishStatusChanged a || publishStatusChanged b
    , noticesChanged = noticesChanged a || noticesChanged b
    }


instance Monoid UIUpdates where
  mempty = emptyUpdates


-- | Empty UI updates.
emptyUpdates :: UIUpdates
emptyUpdates = UIUpdates False False False False False False False False False


-- | Initial effectful QML state.
initialQtQuickState :: QtQuickState
initialQtQuickState = QtQuickState Nothing Nothing initialUIRefs Nothing


-- | Initial UI references.
initialUIRefs :: UIReferences
initialUIRefs = UIReferences Nothing Nothing Nothing Nothing Nothing Nothing Nothing


-- | Define the effects for QML operations.
data QtQuick :: Effect where
  RunEngineLoop :: QML.EngineConfig -> QML.SignalKey (IO ()) -> QML.ObjRef () -> QtQuick m ()
  CreateSignalKey :: QtQuick m (QML.SignalKey (IO ()))
  FireSignal :: QML.ObjRef () -> QtQuick m ()
  -- object specific signals
  Notify :: UIUpdates -> QtQuick m ()
  NotifyRelayStatus :: QtQuick m ()


type instance DispatchOf QtQuick = Dynamic

makeEffect ''QtQuick


-- | Handler for the QML effects.
runQtQuick
  :: (IOE :> es, Concurrent :> es, Logging :> es, State QtQuickState :> es, State AppState :> es)
  => Eff (QtQuick : es) a
  -> Eff es a
runQtQuick = interpret $ \_ -> \case
    RunEngineLoop config changeKey ctx -> do
        q <- newTQueueIO
        put $ QtQuickState (Just changeKey) (Just ctx) initialUIRefs (Just q)
        void $ async $ forever $ do
          uiUpdates <- atomically $ readTQueue q
          moreUpdates <- atomically $ flushTQueue q
          let combinedUpdates = uiUpdates <> mconcat moreUpdates

          refs <- gets uiRefs
          -- Define update checks and their corresponding refs
          let updates = [ (profilesChanged, profileObjRef)
                        , (followsChanged, followsObjRef)
                        , (postsChanged, postsObjRef)
                        , (privateMessagesChanged, privateMessagesObjRef)
                        , (dmRelaysChanged, dmRelaysObjRef)
                        , (generalRelaysChanged, generalRelaysObjRef)
                        , (tempRelaysChanged, tempRelaysObjRef)
                        ]

          forM_ updates $ \(checkFn, getRef) ->
            when (checkFn combinedUpdates) $
              forM_ (getRef refs) (liftIO . QML.fireSignal changeKey)

          threadDelay 200000  -- 0.2 second delay for UI updates

        liftIO $ QML.runEngineLoop config

    CreateSignalKey -> liftIO $ QML.newSignalKey

    FireSignal obj -> do
        st <- get
        case signalKey st of
          Just key -> liftIO $ QML.fireSignal key obj
          Nothing -> logError "No signal key available"

    NotifyRelayStatus -> do
        st <- get
        case queue st of
          Just q -> do
            let updates = emptyUpdates { dmRelaysChanged = True, generalRelaysChanged = True, tempRelaysChanged = True }
            atomically $ writeTQueue q updates
          Nothing -> logError "No queue available"

    Notify u -> do
        st <- get
        case queue st of
          Just q -> atomically $ writeTQueue q u
          Nothing -> logError "No queue available"
