{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Web.Templar.Cache.Memory
where

import ClassyPrelude
import Web.Templar.Cache

memCache :: forall k v. (Hashable k, Eq k) => IO (Cache k v)
memCache = do
    cacheVar <- newMVar (mapFromList [] :: HashMap k v)
    return
        Cache
            { cacheGet = \key ->
                lookup key <$> readMVar cacheVar
            , cachePut = \key val ->
                modifyMVar_ cacheVar $ return . insertMap key val
            , cacheDelete = \key ->
                modifyMVar_ cacheVar $ return . deleteMap key
            }
