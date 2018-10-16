{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Web.Sprinkles.Cache.Memory
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Cache
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as HashMap

memCache :: forall k v. (Hashable k, Eq k) => POSIXTime -> IO (Cache k v)
memCache maxAge = do
    cacheVar <- newMVar (mapFromList [] :: HashMap k (v, POSIXTime))
    return
        Cache
            { cacheGet = \key ->
                fmap fst . lookup key <$> readMVar cacheVar
            , cachePut = \key val -> do
                ts <- getPOSIXTime
                modifyMVar_ cacheVar $ return . insertMap key (val, ts)
            , cacheDelete = \key ->
                modifyMVar_ cacheVar $ return . deleteMap key
            , cacheVacuum = do
                now <- getPOSIXTime
                let threshold = now - maxAge
                modifyMVar cacheVar $ \m -> do
                    let m' = HashMap.filter (\(_, ts) -> ts > threshold) m
                        sizeBefore = HashMap.size m
                        sizeAfter = HashMap.size m'
                    return (m', sizeBefore - sizeAfter)
            }
