{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Web.Templar.Cache
where

import ClassyPrelude
import Control.MaybeEitherMonad

-- | Common interface for cache backends.
-- In @Cache k v@, @k@ is the key type and @v@ the value type.
data Cache k v =
    Cache
        { cacheGet :: k -> IO (Maybe v) -- ^ Get 'Just' the cached value or 'Nothing'
        , cachePut :: k -> v -> IO () -- ^ Insert an entry into the cache
        , cacheDelete :: k -> IO () -- ^ Delete an entry from the cache
        }

instance Monoid (Cache k v) where
    mempty = nullCache
    mappend = appendCache

-- | @cacheFetch load cache key@ fetches a value at the @key@ from the @cache@,
-- failing over to the @load@ action when the key is not present in the cache,
-- and adding it to the cache.
cacheFetch :: (k -> IO v) -> Cache k v -> k -> IO v
cacheFetch load cache key = do
    valueMay <- cacheGet cache key
    case valueMay of
        Just value ->
            return value
        Nothing -> do
            value <- load key
            cachePut cache key value
            return value

cached :: Cache k v -> (k -> IO v) -> k -> IO v
cached = flip cacheFetch

-- | A cache that doesn't actually cache anything
nullCache :: Cache k v
nullCache =
    Cache
        { cacheGet = const $ return Nothing
        , cachePut = const . const $ return ()
        , cacheDelete = const $ return ()
        }

appendCache :: Cache k v -> Cache k v -> Cache k v
appendCache first second =
    Cache
        { cacheGet = \key -> do
            cacheGet first key >>= \case
                Nothing ->
                    cacheGet second key >>=
                        maybe
                            (return Nothing)
                            (\value -> do
                                cachePut first key value
                                return $ Just value
                            )
                Just value ->
                    return $ Just value
        , cachePut = \key value -> do
            cachePut first key value
            cachePut second key value
        , cacheDelete = \key -> do
            cacheDelete first key
            cacheDelete second key
        }

-- | Wrap a cache such that the new cache uses different types for the keys
-- and values, using the provided transformation functions. A typical
-- application for this is to serialize data structures into a more palatable
-- format for caching while exposing a more useful kind of data structure
transformCache :: (k -> j)
               -> (v -> IO (Maybe u))
               -> (u -> IO (Maybe v))
               -> Cache j u
               -> Cache k v
transformCache transK
               transV
               untransV
               innerCache =
    Cache
        { cacheGet = \key -> do
            cacheGet innerCache (transK key) >>= \case
                Nothing -> return Nothing
                Just tval -> untransV tval
        , cachePut = \key value ->
            transV value >>= optionally (cachePut innerCache (transK key))
        , cacheDelete = \key -> cacheDelete innerCache (transK key)
        }
