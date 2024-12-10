{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Store.LMDB where

import Data.ByteString.Lazy qualified as LBS
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Shared (State)
import Effectful.State.Static.Shared qualified as State
import Effectful.FileSystem
import Effectful.TH (makeEffect)
import Lmdb.Types
import Lmdb.Connection
import Lmdb.Map qualified as Map
import Lmdb.Codec qualified as Codec
import System.FilePath ((</>))

import Logging
import Types (AppState(..))

-- | Default LMDB settings for JSON-serializable types
defaultJsonSettings :: (Ord k, ToJSON k, FromJSON k, ToJSON v, FromJSON v) 
                   => DatabaseSettings k v
defaultJsonSettings = makeSettings
    (SortCustom $ CustomSortSafe compare)
    (Codec.throughByteString
        (LBS.toStrict . encode)
        (decode . LBS.fromStrict))
    (Codec.throughByteString
        (LBS.toStrict . encode)
        (decode . LBS.fromStrict))

-- LMDB configuration
maxMapSize :: Int
maxMapSize = 500_000_000_000 -- 500 GB

maxReaders :: Int
maxReaders = 120

maxDbs :: Int
maxDbs = 120

-- | Initialize the LMDB environment
initializeEnv :: FilePath -> IO (Environment ReadWrite)
initializeEnv dbPath = 
    initializeReadWriteEnvironment maxMapSize maxReaders maxDbs dbPath
