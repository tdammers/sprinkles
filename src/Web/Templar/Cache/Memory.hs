{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Web.Templar.Cache.Memory
where

import ClassyPrelude
import Web.Templar.Cache
import Data.Time.Clock.POSIX

memCache :: forall k v. (Hashable k, Eq k) => IO (Cache k v)
memCache = do
    cacheVar <- newMVar (mapFromList [] :: HashMap k (v, POSIXTime))
    return
        Cache
            { cacheGet = \key ->
                lookup key <$> readMVar cacheVar
            , cachePut = \key val -> do
                ts <- getPOSIXTime
                modifyMVar_ cacheVar $ return . insertMap key (val, ts)
            , cacheDelete = \key ->
                modifyMVar_ cacheVar $ return . deleteMap key
            , cacheKeys = do
                pairs <- mapToList <$> readMVar cacheVar
                return [ (k, ts) | (k, (_, ts)) <- pairs ]
            }
