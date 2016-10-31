{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}
module Web.Sprinkles.ServerConfig
where

import ClassyPrelude
import Web.Sprinkles.Rule
import Data.Aeson as JSON
import Data.Aeson.TH
import qualified Data.Yaml as YAML
import Web.Sprinkles.Backends
import Data.Default
import System.FilePath.Glob (glob)
import System.Environment (getEnv, lookupEnv)
import Control.MaybeEitherMonad (maybeFail)
import System.Directory (doesFileExist)
import Data.Scientific (Scientific)
import Data.Time.Clock.POSIX (POSIXTime)
import Web.Sprinkles.Logger (LogLevel (..))
import Web.Sprinkles.Exceptions

data BackendCacheConfig =
    FilesystemCache FilePath POSIXTime |
    MemCache POSIXTime |
    MemcachedCache
    deriving (Show)

instance FromJSON BackendCacheConfig where
    parseJSON (String str) = maybeFail $ backendCacheConfigFromString str
    parseJSON (Object obj) = (obj .: "type") >>= \case
        "file" -> FilesystemCache <$>
                    (obj .:? "dir" .!= ".cache") <*>
                    (obj .:? "max-age" .!= 300)
        "mem" -> MemCache <$>
                    (obj .:? "max-age" .!= 60)
        "memcached" -> return MemcachedCache
        x -> fail $ "Invalid backend cache type: '" <> x
    parseJSON x = fail $ "Invalid backend cache specification: " <> show x

backendCacheConfigFromString :: Text -> Maybe BackendCacheConfig
backendCacheConfigFromString str =
    case splitSeq ":" str of
        ["file", dir] -> return $ FilesystemCache (unpack dir) 300
        ["file"] -> return $ FilesystemCache ".cache" 300
        ["mem"] -> return $ MemCache 60
        ["memcached"] -> return MemcachedCache
        xs -> Nothing

data ServerDriver = WarpDriver (Maybe Int)
                  | CGIDriver
                  | SCGIDriver
                  | FastCGIDriver
                  | DefaultDriver
    deriving (Show, Read, Eq)

instance Default ServerDriver where
    def = DefaultDriver

instance FromJSON ServerDriver where
    parseJSON (String "warp") =
        return $ WarpDriver Nothing
    parseJSON (String "cgi") =
        return CGIDriver
    parseJSON (String "fastcgi") =
        return FastCGIDriver
    parseJSON (String "fcgi") =
        return FastCGIDriver
    parseJSON (String "scgi") =
        return SCGIDriver
    parseJSON (String "default") =
        return def
    parseJSON (Object o) = do
        st :: Text <- o .: "type"
        case st of
            "warp" -> WarpDriver <$> o .:? "port"
            "cgi" -> return CGIDriver
            "fcgi" -> return FastCGIDriver
            "fastcgi" -> return FastCGIDriver
            "scgi" -> return SCGIDriver

data LoggerConfig =
    DiscardLog |
    Syslog LogLevel |
    StdioLog LogLevel
    deriving (Show)

instance FromJSON LoggerConfig where
    parseJSON (Object obj) = do
        dest <- obj .:? "destination"
        case dest :: Maybe Text of
            Nothing -> return DiscardLog
            Just "discard" -> return DiscardLog
            Just "null" -> return DiscardLog
            Just "stdio" ->
                StdioLog . fromMaybe Warning <$> obj .:? "level"
            Just "syslog" ->
                Syslog . fromMaybe Warning <$> obj .:? "level"

    parseJSON _ = fail "Invalid logger config"

data ServerConfig =
    ServerConfig
        { scBackendCache :: [BackendCacheConfig]
        , scDriver :: ServerDriver
        , scLogger :: Maybe LoggerConfig
        }
        deriving (Show)

instance Default ServerConfig where
    def = ServerConfig
            { scBackendCache = def
            , scDriver = def
            , scLogger = Nothing
            }

instance Monoid ServerConfig where
    mempty = def
    mappend = scAppend

instance FromJSON ServerConfig where
    parseJSON (Object obj) = do
        caches <- fromMaybe []
                    <$> ( obj .:? "backend-cache"
                          <|> (fmap (:[]) <$> obj .:? "backend-cache")
                        )
        driver <- fromMaybe def
                    <$> ( obj .:? "driver" )
        logger <- obj .:? "log"
        return ServerConfig
            { scBackendCache = caches
            , scDriver = driver
            , scLogger = logger
            }
    parseJSON _ = fail "Invalid server config"

scAppend :: ServerConfig -> ServerConfig -> ServerConfig
scAppend a b =
    ServerConfig
        { scBackendCache =
            firstNonNull (scBackendCache b) (scBackendCache a)
        , scLogger = scLogger b <|> scLogger a
        , scDriver =
            if scDriver b == DefaultDriver
                then scDriver a
                else scDriver b
        }

firstNonNull :: [a] -> [a] -> [a]
firstNonNull [] xs = xs
firstNonNull xs _ = xs

loadServerConfigFile :: FilePath -> IO ServerConfig
loadServerConfigFile fn =
    YAML.decodeFileEither fn >>=
        either
            (throwM . withSourceContext (pack fn))
            return

loadServerConfig :: FilePath -> IO ServerConfig
loadServerConfig dir = do
    homeDirMay <- lookupEnv "HOME"
    let systemGlobalFilename = "/etc/sprinkles/server.yml"
        globalFilename = "/usr/local/etc/sprinkles/server.yml"
        userFilenameMay = (</> ".config" </> "sprinkles" </> "server.yml") <$> homeDirMay
        localFilename = dir </> "config" </> "server.yml"
        serverConfigFilename = dir </> "server.yml"
    let filenames' = catMaybes
            [ Just systemGlobalFilename
            , Just globalFilename
            , userFilenameMay
            , Just localFilename
            , Just serverConfigFilename
            ]
    filenames <- filterM doesFileExist filenames'
    mconcat <$> forM filenames loadServerConfigFile