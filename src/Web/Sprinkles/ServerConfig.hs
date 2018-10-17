{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE TypeSynonymInstances #-}
module Web.Sprinkles.ServerConfig
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Rule
import Data.Aeson as JSON
import Data.Aeson.TH
import qualified Data.Yaml as YAML
import Web.Sprinkles.Backends
import Data.Default
import System.FilePath.Glob (glob)
import Control.MaybeEitherMonad (maybeFail)
import System.Directory (doesFileExist)
import Data.Scientific (Scientific)
import Data.Time.Clock.POSIX (POSIXTime)
import Web.Sprinkles.Logger (LogLevel (..))
import Web.Sprinkles.Exceptions
import Web.Sprinkles.Databases (DSN (..), SqlDriver (..))

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
                  | BakeDriver
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
    parseJSON (String "bake") =
        return BakeDriver
    parseJSON (String "default") =
        return def
    parseJSON (String x) =
        fail $ "Invalid server driver " ++ show x
    parseJSON (Object o) = do
        st :: Text <- o .: "type"
        case st of
            "warp" -> WarpDriver <$> o .:? "port"
            "cgi" -> return CGIDriver
            "fcgi" -> return FastCGIDriver
            "fastcgi" -> return FastCGIDriver
            "scgi" -> return SCGIDriver
            "bake" -> return BakeDriver
            x -> fail $ "Invalid server driver " ++ show x
    parseJSON x =
        fail $ "Invalid server driver " ++ show x

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
            Just x -> fail $ "Invalid logger type " ++ show x
    parseJSON _ = fail "Invalid logger config"

data SessionExpiration = NeverExpire 
                       | SlidingExpiration Integer
                       deriving (Show)

instance FromJSON SessionExpiration where
    parseJSON = \case
        Number n -> return . SlidingExpiration . floor $ n
        Null -> return NeverExpire
        String "never" -> return NeverExpire
        _ -> fail "Invalid session expiration"

data SessionDriver = NoSessionDriver
                   | InProcSessionDriver
                   | SqlSessionDriver DSN
                   deriving (Show)

instance FromJSON SessionDriver where
    parseJSON = \case
        Null -> return NoSessionDriver
        String "inproc" -> return InProcSessionDriver
        String "sql" -> return $ SqlSessionDriver (DSN SqliteDriver "sessions.sqlite")
        String x -> fail $ "Invalid session driver " ++ show x
        Object obj -> do
            ty <- obj .: "type"
            case (ty :: Text) of
                "inproc" ->
                    return InProcSessionDriver
                "sql" -> do
                    SqlSessionDriver <$>
                        obj .:? "connection" .!= DSN SqliteDriver "sessions.sqlite"
                x -> fail $ "Invalid session driver " ++ show x
        _ -> fail "Invalid session driver"

data SessionConfig =
    SessionConfig
        { sessCookieName :: ByteString
        , sessCookieSecure :: Bool
        , sessExpiration :: SessionExpiration
        , sessDriver :: SessionDriver
        }
        deriving (Show)

instance FromJSON SessionConfig where
    parseJSON (Object obj) = do
        cookieName <- encodeUtf8 <$> obj .:? "cookie-name" .!= "ssid"
        expiration <- obj .:? "expiration" .!= NeverExpire
        secure <- obj .:? "secure" .!= True -- secure default
        driver <- obj .:? "driver" .!= InProcSessionDriver
        return $ SessionConfig cookieName secure expiration driver
    parseJSON x = do
        driver <- parseJSON x
        return $ SessionConfig "ssid" True NeverExpire driver

instance Default SessionConfig where
    def = SessionConfig "ssid" True NeverExpire NoSessionDriver

data ServerConfig =
    ServerConfig
        { scBackendCache :: [BackendCacheConfig]
        , scDriver :: ServerDriver
        , scLogger :: Maybe LoggerConfig
        , scSessions :: SessionConfig
        , scRootDir :: FilePath
        }
        deriving (Show)

instance Default ServerConfig where
    def = ServerConfig
            { scBackendCache = def
            , scDriver = def
            , scLogger = Nothing
            , scSessions = def
            , scRootDir = ""
            }

instance Semigroup ServerConfig where
    (<>) = scAppend

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
        sessions <- obj .:? "sessions" .!= def
        rootDir <- obj .:? "dir" .!= ""
        return ServerConfig
            { scBackendCache = caches
            , scDriver = driver
            , scLogger = logger
            , scSessions = sessions
            , scRootDir = rootDir
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
        , scSessions =
            case sessDriver (scSessions b) of
                NoSessionDriver -> scSessions a
                _ -> scSessions b
        , scRootDir = 
            firstNonNull (scRootDir b) (scRootDir a)
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
    homeDirMay <- lookupEnv ("HOME" :: String)
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
