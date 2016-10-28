{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Sprinkles.Cache.Memcached
where

import ClassyPrelude
import Web.Sprinkles.Cache
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as HashMap
import Data.Default
import qualified Database.Memcached.Binary.Maybe as Memcached

memcachedCache :: IO (Cache ByteString ByteString)
memcachedCache = do
    let connectInfo :: Memcached.ConnectInfo = def
        withConnection = Memcached.withConnection connectInfo
        expiry = 60
    return
        Cache
            { cacheGet = \key -> do
                lazyVal <- withConnection $ Memcached.get_ key
                return $ toStrict <$> lazyVal
            , cachePut = \key val -> do
                withConnection $ Memcached.set 0 expiry key (fromStrict val)
                return ()
            , cacheDelete = \key -> do
                withConnection $ Memcached.delete key
                return ()
            , cacheVacuum = return 0
            }
