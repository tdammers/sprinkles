{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Backend spec types and parser
module Web.Templar.Backends.Spec
(
-- * Defining backends
  BackendSpec (..)
, BackendType (..)
, FetchOrderField (..)
, FetchMode (..)
, AscDesc (..)
, FetchOrder (..)
, parseBackendURI
, Credentials (..)
)
where

import ClassyPrelude
import Network.Mime (MimeType)
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import System.PosixCompat.Files
import Data.Default (Default (..))
import Web.Templar.Cache
import qualified Data.Serialize as Cereal
import Data.Serialize (Serialize)
import Web.Templar.Databases (DSN (..), sqlDriverFromID)
import Web.Templar.Logger (LogLevel (..))

-- | A type of backend.
data BackendType = HttpBackend Text Credentials -- ^ Fetch data over HTTP(S)
                 | FileBackend Text -- ^ Read local files
                 | SqlBackend DSN Text [Text] -- ^ Query an SQL database
                 | SubprocessBackend Text [Text] MimeType -- ^ Run a command in a subprocess
                 deriving (Show, Generic)

instance Serialize BackendType where
    put (HttpBackend url credentials) = do
        Cereal.put 'h'
        Cereal.put (encodeUtf8 . unpack $ url)
        Cereal.put credentials
    put (FileBackend path) = do
        Cereal.put 'f'
        Cereal.put (encodeUtf8 . unpack $ path)
    put (SqlBackend dsn query params) = do
        Cereal.put 's'
        Cereal.put dsn
        Cereal.put (encodeUtf8 . unpack $ query)
        Cereal.put (map (encodeUtf8 . unpack) params)
    put (SubprocessBackend cmd args t) = do
        Cereal.put 'p'
        Cereal.put (encodeUtf8 . unpack $ cmd)
        Cereal.put (map (encodeUtf8 . unpack) args)
        Cereal.put t
    get = do
        Cereal.get >>= \case
            'h' -> HttpBackend <$> (pack . decodeUtf8 <$> Cereal.get) <*> Cereal.get
            'f' -> FileBackend <$> (pack . decodeUtf8 <$> Cereal.get)
            's' -> SqlBackend <$>
                    Cereal.get <*>
                    (pack . decodeUtf8 <$> Cereal.get) <*>
                    (map (pack . decodeUtf8) <$> Cereal.get)
            'p' -> SubprocessBackend <$>
                    (pack . decodeUtf8 <$> Cereal.get) <*>
                    (map (pack . decodeUtf8) <$> Cereal.get) <*>
                    Cereal.get
            x -> fail $ "Invalid backend type identifier: " <> show x

type instance Element BackendType = Text

instance MonoFunctor BackendType where
    omap f (HttpBackend t c) = HttpBackend (f t) c
    omap f (FileBackend t) = FileBackend (f t)
    omap f (SqlBackend dsn query params) = SqlBackend (omap f dsn) query (map f params)
    omap f (SubprocessBackend cmd args t) = SubprocessBackend cmd (map f args) t

-- | A specification of a backend query.
data BackendSpec =
    BackendSpec
        { bsType :: BackendType -- ^ Defines the data source
        , bsFetchMode :: FetchMode -- ^ How many items to fetch, and in what shape
        , bsOrder :: FetchOrder -- ^ How to order items
        }
        deriving (Show, Generic)

instance Serialize BackendSpec

type instance Element BackendSpec = Text

instance MonoFunctor BackendSpec where
    omap f (BackendSpec t m o) = BackendSpec (omap f t) m o

-- | The JSON shape of a backend spec is:
--
-- @
-- {
--   // type: one of:
--   // - "http" (fetch over HTTP)
--   // - "https" (fetch over HTTPS)
--   // - "file" (load an individual file)
--   // - "glob" (resolve a glob and load all matching files)
--   // - "dir" (get a directory listing)
--   // - "sql" (query an SQL database)
--   "type": type,
--
--   // fetch mode. One of:
--   - "one": Fetch exactly one item, as a scalar
--   - "all": Fetch all items, as a list
--   - n (numeric value): Fetch up to n items, as a list
--   "fetch": fetchMode,
--
--   // ordering. One of:
--   // - "arbitrary": do not reorder, use whatever the backend produces
--   // - "random": random-shuffle results
--   // - "shuffle": same as "random"
--   // - "name": order by name
--   // - "mtime": order by modification time
--   // The ordering can be preceded with a "+" or "-" sign to indicate
--   // ascending or descending ordering.
--   "order": ordering,
--
--   // The rest of the structure depends on the type.
--
--   // For "http" and "https":
--   // The HTTP(S) URI to load from
--   "uri": uri,
--
--   // For "file", "glob", "dir":
--   // The local file path or glob
--   "path": path
-- }
-- @
instance FromJSON BackendSpec where
    parseJSON = backendSpecFromJSON

-- | Read a backend spec from a JSON value.
backendSpecFromJSON (String uri) =
    parseBackendURI uri
backendSpecFromJSON (Object obj) = do
    bsTypeStr <- obj .: "type"
    (t, defFetchMode) <- case bsTypeStr :: Text of
            "http" -> parseHttpBackendSpec
            "https" -> parseHttpBackendSpec
            "file" -> parseFileBackendSpec FetchOne
            "glob" -> parseFileBackendSpec FetchAll
            "dir" -> parseDirBackendSpec
            "sql" -> parseSqlBackendSpec
            "subprocess" -> parseSubprocessSpec
    fetchMode <- obj .:? "fetch" .!= defFetchMode
    fetchOrder <- obj .:? "order" .!= def
    return $ BackendSpec t fetchMode fetchOrder
    where
        parseHttpBackendSpec = do
            t <- obj .: "uri"
            return (HttpBackend t AnonymousCredentials, FetchOne)
        parseFileBackendSpec m = do
            path <- obj .: "path"
            return (FileBackend (pack path), m)
        parseDirBackendSpec = do
            path <- obj .: "path"
            return (FileBackend (pack $ path </> "*"), FetchAll)
        parseSqlBackendSpec = do
            dsn <- obj .: "connection"
            query <- obj .: "query"
            params <- obj .:? "params" .!= []
            return (SqlBackend dsn query params, FetchAll)
        parseSubprocessSpec = do
            rawCmd <- obj .: "cmd"
            t <- fromString <$> (obj .:? "mime-type" .!= "text/plain")
            case rawCmd of
                String cmd -> return (SubprocessBackend cmd [] t, FetchOne)
                Array v -> parseJSON rawCmd >>= \case
                    cmd:args -> return (SubprocessBackend cmd args t, FetchOne)
                    [] -> fail "Expected a command and a list of arguments"
backendSpecFromJSON x = fail $ "Invalid JSON value for BackendSpec: " <> show x <> ", expecting object or string"

-- | Parse a 'Text' into a 'BackendSpec'.
parseBackendURI :: Monad m => Text -> m BackendSpec
parseBackendURI t = do
    let protocol = takeWhile (/= ':') t
        path = drop (length protocol + 3) t
    case protocol of
        "http" ->
            return $
                BackendSpec
                    (HttpBackend t AnonymousCredentials)
                    FetchOne
                    def
        "https" ->
            return $
                BackendSpec
                    (HttpBackend t AnonymousCredentials)
                    FetchOne
                    def
        "dir" -> return $ BackendSpec (FileBackend (pack $ unpack path </> "*")) FetchAll def
        "glob" -> return $ BackendSpec (FileBackend path) FetchAll def
        "file" -> return $ BackendSpec (FileBackend path) FetchOne def
        "sql" -> do
            be <- parseSqlBackendURI path
            return $ BackendSpec be FetchAll def
        _ -> fail $ "Unknown protocol: " <> show protocol
    where
        parseSqlBackendURI path = do
            let driverID = takeWhile (/= ':') path
                remainder = drop (length driverID + 1) path
                details = takeWhile (/= ':') remainder
                query = drop (length details + 1) remainder
            driver <- maybe
                (fail $ "Invalid driver: " ++ show driverID)
                return
                (sqlDriverFromID driverID)
            return $ SqlBackend (DSN driver details) query []

-- | How many items to fetch, and in what shape.
data FetchMode = FetchOne -- ^ Fetch only the first result
               | FetchAll -- ^ Fetch all results
               | FetchN Int -- ^ Fetch at most @n@ results, starting from the top
    deriving (Show, Read, Eq, Generic)

instance Serialize FetchMode where

instance FromJSON FetchMode where
    parseJSON (String "one") = return FetchOne
    parseJSON (String "all") = return FetchAll
    parseJSON (Number n) = return . FetchN . ceiling $ n
    parseJSON _ = fail "Invalid fetch mode (want 'one' or 'all')"

-- | By which field should we order results?
data FetchOrderField = ArbitraryOrder -- ^ Do not impose any ordering at all
                     | RandomOrder -- ^ Shuffle randomly
                     | OrderByName -- ^ Order by reported name
                     | OrderByMTime -- ^ Order by modification time
                     deriving (Show, Read, Eq, Generic)

instance Serialize FetchOrderField where

instance Default FetchOrderField where
    def = ArbitraryOrder

data AscDesc = Ascending | Descending
    deriving (Show, Read, Eq, Generic)

instance Serialize AscDesc where

instance Default AscDesc where
    def = Ascending

-- | How to order results.
data FetchOrder =
    FetchOrder
        { fetchField :: FetchOrderField -- ^ By which field?
        , fetchAscDesc :: AscDesc -- ^ Reverse ordering?
        }
        deriving (Show, Read, Eq, Generic)

instance Serialize FetchOrder where

instance Default FetchOrder where
    def = FetchOrder def def

instance FromJSON FetchOrder where
    parseJSON Null = return $ FetchOrder ArbitraryOrder Ascending
    parseJSON (String str) = do
        let (order, core) = case take 1 str of
                "-" -> (Descending, drop 1 str)
                "+" -> (Ascending, drop 1 str)
                _ -> (Ascending, str)
        field <- case core of
            "arbitrary" -> return ArbitraryOrder
            "random" -> return RandomOrder
            "shuffle" -> return RandomOrder
            "name" -> return OrderByName
            "mtime" -> return OrderByMTime
            x -> fail $ "Invalid order field: " ++ show x
        return $ FetchOrder field order
    parseJSON val = fail $ "Invalid fetch order specifier: " ++ show val

-- | Credentials to pass to an external backend data source. Currently stubbed,
-- supporting only anonymous access.
data Credentials = AnonymousCredentials
    deriving (Show, Generic)

instance Serialize Credentials where

instance FromJSON Credentials where
    parseJSON Null = return AnonymousCredentials
    parseJSON (String "anonymous") = return AnonymousCredentials
    parseJSON _ = fail "Invalid credentials"

