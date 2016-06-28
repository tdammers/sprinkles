{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Backend type and implementations.
module Web.Templar.Backends
(
-- * Defining backends
  BackendSpec
, parseBackendURI
-- * Fetching backend data
, BackendData (..)
, BackendMeta (..)
, Items (..)
, loadBackendData
, RawBackendCache
)
where

import ClassyPrelude
import Data.Default (Default (..))
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import Network.Mime
            ( MimeType
            , MimeMap
            , defaultMimeLookup
            , defaultMimeMap
            , mimeByExt
            , defaultMimeType
            , FileName
            )
import Text.Ginger (ToGVal (..), GVal, Run (..), dict, (~>))
import Web.Templar.PandocGVal
import System.FilePath (takeFileName, takeBaseName)
import System.FilePath.Glob (glob)
import Foreign.C.Types (CTime (..))
import Data.Char (ord)
import qualified Text.Ginger as Ginger
import System.Random.Shuffle (shuffleM)
import Web.Templar.Cache
import qualified Data.Serialize as Cereal
import Data.Serialize (Serialize)
import Control.MaybeEitherMonad (eitherFailS)
import Web.Templar.Logger (LogLevel (..))
import qualified System.Process as Process

import Web.Templar.Backends.Spec
        ( BackendSpec (..)
        , BackendType (..)
        , AscDesc (..)
        , FetchMode (..)
        , FetchOrder (..)
        , FetchOrderField (..)
        , parseBackendURI
        )
import Web.Templar.Backends.Parsers
        ( parseBackendData
        )
import Web.Templar.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        )
import Web.Templar.Backends.Loader

-- | Cache for raw backend data, stored as bytestrings.
type RawBackendCache = Cache ByteString ByteString

-- | Well-typed backend cache.
type BackendCache = Cache BackendSpec [BackendSource]

-- | Execute a backend query, with caching.
loadBackendData :: (LogLevel -> Text -> IO ()) -> RawBackendCache -> BackendSpec -> IO (Items (BackendData m h))
loadBackendData writeLog cache bspec =
    fmap (reduceItems (bsFetchMode bspec)) $
        fetchBackendData writeLog cache bspec >>=
        mapM parseBackendData >>=
        sorter
    where
        sorter :: [BackendData m h] -> IO [BackendData m h]
        sorter = fmap reverter . baseSorter
        reverter :: [a] -> [a]
        reverter = case fetchAscDesc (bsOrder bspec) of
            Ascending -> id
            Descending -> reverse
        baseSorter :: [BackendData m h] -> IO [BackendData m h]
        baseSorter = case fetchField (bsOrder bspec) of
            ArbitraryOrder -> return
            RandomOrder -> shuffleM
            OrderByName -> return . sortOn (bmName . bdMeta)
            OrderByMTime -> return . sortOn (bmMTime . bdMeta)

-- | What the type says: expose a raw backend cache (bytestrings) as a
-- well-typed backend cache.
wrapBackendCache :: RawBackendCache -> BackendCache
wrapBackendCache =
    transformCache
        Cereal.encode
        (return . Just . Cereal.encode)
        (fmap Just . eitherFailS . Cereal.decode)

-- | Fetch raw backend data from a backend source, with caching.
fetchBackendData :: (LogLevel -> Text -> IO ()) -> RawBackendCache -> BackendSpec -> IO [BackendSource]
fetchBackendData writeLog rawCache =
    cached cache (fetchBackendData' writeLog)
    where
        cache :: BackendCache
        cache = wrapBackendCache rawCache

-- | Fetch raw backend data from a backend source, without caching.
fetchBackendData' :: (LogLevel -> Text -> IO ()) -> BackendSpec -> IO [BackendSource]
fetchBackendData' writeLog (BackendSpec backendType fetchMode fetchOrder) =
    loader backendType writeLog fetchMode fetchOrder
