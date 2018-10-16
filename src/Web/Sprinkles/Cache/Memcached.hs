{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Sprinkles.Cache.Memcached
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Cache
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as HashMap
import Data.Default
import qualified Database.Memcache.Client as Memcache

memcachedCache :: IO (Cache ByteString ByteString)
memcachedCache = do
    let options :: Memcache.Options = def
        withConnection =
          bracket
            (Memcache.newClient [] options)
            (Memcache.quit)
        expiry = 60
    return
        Cache
            { cacheGet = \key -> do
                withConnection $ \client -> Memcache.gat client key expiry >>= \case
                  Just (val, _, _) -> return (Just val)
                  Nothing -> return Nothing
            , cachePut = \key val -> do
                withConnection $ \client ->
                  Memcache.set client key val 0 expiry
                return ()
            , cacheDelete = \key -> do
                withConnection $ \client ->
                  Memcache.delete client key 0
                return ()
            , cacheVacuum = return 0
            }
