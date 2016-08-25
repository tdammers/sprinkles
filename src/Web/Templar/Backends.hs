{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Main Backend module.
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
import System.Random.Shuffle (shuffleM)
import Web.Templar.Cache
import qualified Data.Serialize as Cereal
import Control.MaybeEitherMonad (eitherFailS)
import Web.Templar.Logger (LogLevel (..))
import Network.Mime (MimeType)

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
        (eitherFailS . Cereal.decode)
        (return . Just . Cereal.encode)
        (fmap Just . eitherFailS . Cereal.decode)

-- | Fetch raw backend data from a backend source, with caching.
fetchBackendData :: (LogLevel -> Text -> IO ()) -> RawBackendCache -> BackendSpec -> IO [BackendSource]
fetchBackendData writeLog rawCache spec = do
    cached cache (fetchBackendData' writeLog) spec
    where
        cache :: BackendCache
        cache = wrapBackendCache rawCache

-- | Fetch raw backend data from a backend source, without caching.
fetchBackendData' :: (LogLevel -> Text -> IO ()) -> BackendSpec -> IO [BackendSource]
fetchBackendData' writeLog (BackendSpec backendType fetchMode fetchOrder mimeOverride) = do
    map (overrideMime mimeOverride) <$> loader backendType writeLog fetchMode fetchOrder

overrideMime :: Maybe MimeType -> BackendSource -> BackendSource
overrideMime Nothing s = s
overrideMime (Just m) s =
    s { bsMeta = (bsMeta s) { bmMimeType = m } }
