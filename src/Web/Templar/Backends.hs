{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Backend type and implementations.
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
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import qualified Network.HTTP as HTTP
import Network.Mime
            ( MimeType
            , MimeMap
            , defaultMimeLookup
            , defaultMimeMap
            , mimeByExt
            , defaultMimeType
            , FileName
            )
import Network.URI (parseURI, URI)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc (Pandoc)
import Text.Pandoc.Error (PandocError)
import Text.Ginger (ToGVal (..), GVal, Run (..), dict, (~>))
import Web.Templar.PandocGVal
import System.FilePath (takeFileName, takeBaseName)
import System.FilePath.Glob (glob)
import System.PosixCompat.Files
import Foreign.C.Types (CTime (..))
import Data.Char (ord)
import qualified Text.Ginger as Ginger
import Data.Default (def)
import System.Random.Shuffle (shuffleM)
import Data.Default
import Web.Templar.Cache
import qualified Data.Serialize as Cereal
import Data.Serialize (Serialize)
import Control.MaybeEitherMonad (eitherFailS)
import qualified Web.Templar.Databases as DB
import Web.Templar.Databases (DSN (..), sqlDriverFromID)
import qualified Database.HDBC as HDBC

-- | Our own extended MIME type dictionary. The default one lacks appropriate
-- entries for some of the important types we use, so we add them here.
mimeMap :: MimeMap
mimeMap =
    defaultMimeMap <>
    mapFromList
        [ ("yml", "application/x-yaml")
        , ("yaml", "application/x-yaml")
        , ("md", "application/x-markdown")
        , ("rst", "text/x-rst")
        , ("textile", "text/x-textile")
        , ("markdown", "application/x-markdown")
        , ("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
        ]

-- | Custom MIME lookup function that knows about the extra types declared in
-- 'mimeMap'.
mimeLookup :: FileName -> MimeType
mimeLookup = mimeByExt mimeMap defaultMimeType

-- | A type of backend.
data BackendType = HttpBackend Text Credentials -- ^ Fetch data over HTTP(S)
                 | FileBackend Text -- ^ Read local files
                 | SqlBackend DSN Text [Text]
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
    get = do
        Cereal.get >>= \case
            'h' -> HttpBackend <$> (pack . decodeUtf8 <$> Cereal.get) <*> Cereal.get
            'f' -> FileBackend <$> (pack . decodeUtf8 <$> Cereal.get)
            's' -> SqlBackend <$>
                    Cereal.get <*>
                    (pack . decodeUtf8 <$> Cereal.get) <*>
                    (map (pack . decodeUtf8) <$> Cereal.get)
            x -> fail $ "Invalid backend type identifier: " <> show x

type instance Element BackendType = Text

instance MonoFunctor BackendType where
    omap f (HttpBackend t c) = HttpBackend (f t) c
    omap f (FileBackend t) = FileBackend (f t)
    omap f (SqlBackend dsn query params) = SqlBackend dsn query (map f params)

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
            dsn <- obj .: "dsn"
            query <- obj .: "query"
            params <- obj .:? "params" .!= []
            return (SqlBackend dsn query params, FetchAll)
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

-- | The shapes of data that can be returned from a backend query.
data Items a = NotFound -- ^ Nothing was found
             | SingleItem a -- ^ A single item was requested, and this is it
             | MultiItem [a] -- ^ Multiple items were requested, here they are

-- | Transform a raw list of results into an 'Items' value. This allows us
-- later to distinguish between Nothing Found vs. Empty List, and between
-- Single Item Requested And Found vs. Many Items Requested, One Found. This
-- is needed such that when a single item is requested, it gets converted to
-- 'GVal' and JSON as a scalar, while when we request many items and receive
-- one, it becomes a singleton list.
reduceItems :: FetchMode -> [a] -> Items a
reduceItems FetchOne [] = NotFound
reduceItems FetchOne (x:_) = SingleItem x
reduceItems FetchAll xs = MultiItem xs
reduceItems (FetchN n) xs = MultiItem $ take n xs

instance ToGVal m a => ToGVal m (Items a) where
    toGVal NotFound = def
    toGVal (SingleItem x) = toGVal x
    toGVal (MultiItem xs) = toGVal xs

instance ToJSON a => ToJSON (Items a) where
    toJSON NotFound = Null
    toJSON (SingleItem x) = toJSON x
    toJSON (MultiItem xs) = toJSON xs

-- | Credentials to pass to an external backend data source. Currently stubbed,
-- supporting only anonymous access.
data Credentials = AnonymousCredentials
    deriving (Show, Generic)

instance Serialize Credentials where

instance FromJSON Credentials where
    parseJSON Null = return AnonymousCredentials
    parseJSON (String "anonymous") = return AnonymousCredentials
    parseJSON _ = fail "Invalid credentials"

-- | A parsed record from a query result.
data BackendData m h =
    BackendData
        { bdJSON :: JSON.Value -- ^ Result body as JSON
        , bdGVal :: GVal (Run m h) -- ^ Result body as GVal
        , bdRaw :: LByteString -- ^ Raw result body source
        , bdMeta :: BackendMeta -- ^ Meta-information
        }

-- | A raw (unparsed) record from a query result.
data BackendSource =
    BackendSource
        { bsMeta :: BackendMeta
        , bsSource :: LByteString
        }
        deriving (Generic)

instance Serialize BackendSource where

-- | Cache for raw backend data, stored as bytestrings.
type RawBackendCache = Cache ByteString ByteString

-- | Well-typed backend cache.
type BackendCache = Cache BackendSpec [BackendSource]

-- | Execute a backend query, with caching.
loadBackendData :: RawBackendCache -> BackendSpec -> IO (Items (BackendData m h))
loadBackendData cache bspec =
    fmap (reduceItems (bsFetchMode bspec)) $
        fetchBackendData cache bspec >>=
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

-- | Wrap a parsed backend value in a 'BackendData' structure. The original
-- raw 'BackendSource' value is needed alongside the parsed value, because the
-- resulting structure contains both the 'BackendMeta' and the raw (unparsed)
-- data from it.
toBackendData :: (ToJSON a, ToGVal (Run m h) a) => BackendSource -> a -> BackendData m h
toBackendData src val =
    BackendData
        { bdJSON = toJSON val
        , bdGVal = toGVal val
        , bdRaw = bsSource src
        , bdMeta = bsMeta src
        }

instance ToJSON (BackendData m h) where
    toJSON = bdJSON

instance ToGVal (Run m h) (BackendData m h) where
    toGVal bd =
        let baseVal = bdGVal bd
            baseLookup = fromMaybe (const def) $ Ginger.asLookup baseVal
            baseDictItems = Ginger.asDictItems baseVal
        in baseVal
            { Ginger.asLookup = Just $ \case
                "props" -> return . toGVal . bdMeta $ bd
                k -> baseLookup k
            , Ginger.asDictItems =
                (("props" ~> bdMeta bd):) <$> baseDictItems
            }

-- | Metadata for a backend query result.
data BackendMeta =
    BackendMeta
        { bmMimeType :: MimeType
        , bmMTime :: Maybe CTime -- ^ Last modification time, if available
        , bmName :: Text -- ^ Human-friendly name
        , bmPath :: Text -- ^ Path, according to the semantics of the backend (file path or URI)
        , bmSize :: Maybe Integer -- ^ Size of the raw source, in bytes, if available
        }
        deriving (Show, Generic)

instance Serialize BackendMeta where
    put bm = do
        Cereal.put $ bmMimeType bm
        Cereal.put . fmap fromEnum $ bmMTime bm
        Cereal.put . encodeUtf8 $ bmName bm
        Cereal.put . encodeUtf8 $ bmPath bm
        Cereal.put $ bmSize bm
    get =
        BackendMeta <$> Cereal.get
                    <*> (fmap toEnum <$> Cereal.get)
                    <*> (decodeUtf8 <$> Cereal.get)
                    <*> (decodeUtf8 <$> Cereal.get)
                    <*> Cereal.get

instance ToJSON BackendMeta where
    toJSON bm =
        JSON.object
            [ "mimeType" .= decodeUtf8 (bmMimeType bm)
            , "mtime" .= (fromIntegral . unCTime <$> bmMTime bm :: Maybe Integer)
            , "name" .= bmName bm
            , "path" .= bmPath bm
            , "size" .= bmSize bm
            ]

instance Ginger.ToGVal m BackendMeta where
    toGVal bm = Ginger.dict
        [ "type" ~> decodeUtf8 (bmMimeType bm)
        , "mtime" ~> (fromIntegral . unCTime <$> bmMTime bm :: Maybe Integer)
        , "name" ~> bmName bm
        , "path" ~> bmPath bm
        , "size" ~> bmSize bm
        ]

-- | What the type says: expose a raw backend cache (bytestrings) as a
-- well-typed backend cache.
wrapBackendCache :: RawBackendCache -> BackendCache
wrapBackendCache =
    transformCache
        Cereal.encode
        (return . Just . Cereal.encode)
        (fmap Just . eitherFailS . Cereal.decode)

-- | Fetch raw backend data from a backend source, with caching.
fetchBackendData :: RawBackendCache -> BackendSpec -> IO [BackendSource]
fetchBackendData rawCache =
    cached cache fetchBackendData'
    where
        cache :: BackendCache
        cache = wrapBackendCache rawCache

-- | Fetch raw backend data from a backend source, without caching.
fetchBackendData' :: BackendSpec -> IO [BackendSource]
fetchBackendData' (BackendSpec (SqlBackend dsn query params) fetchMode fetchOrder) =
    fetch
    where
        fetch = do
            rows <- DB.withConnection dsn $ \conn -> do
                stmt <- HDBC.prepare conn (unpack query)
                HDBC.execute stmt (map HDBC.toSql params)
                HDBC.fetchAllRowsMap stmt
            return $ map mapRow rows
        mapRow :: Map String HDBC.SqlValue -> BackendSource
        mapRow row =
            let json = JSON.encode (fmap (HDBC.fromSql :: HDBC.SqlValue -> Text) row)
                meta = BackendMeta
                        { bmMimeType = "application/json"
                        , bmMTime = Nothing
                        , bmName = "SQL"
                        , bmPath = "SQL"
                        , bmSize = (Just . fromIntegral $ length json)
                        }
            in BackendSource meta json

fetchBackendData' (BackendSpec (FileBackend filepath) fetchMode fetchOrder) =
    fetch `catchIOError` handle
    where
        filename = unpack filepath
        fetch = do
            candidates <- if '*' `elem` filename
                then glob filename
                else return [filename]
            mapM fetchOne candidates
        handle err
            | isDoesNotExistError err = return []
            | otherwise = ioError err

        fetchOne candidate = do
            let mimeType = mimeLookup . pack $ candidate
            contents <- readFile candidate `catchIOError` \err -> do
                -- TODO: inject logger from outside so we can log properly here
                -- hPutStrLn stderr $ show err
                return ""
            status <- getFileStatus candidate
            let mtimeUnix = modificationTime status
                meta = BackendMeta
                        { bmMimeType = mimeType
                        , bmMTime = Just mtimeUnix
                        , bmName = pack $ takeBaseName candidate
                        , bmPath = pack candidate
                        , bmSize = (Just . fromIntegral $ fileSize status :: Maybe Integer)
                        }
            return $ BackendSource meta contents
fetchBackendData' (BackendSpec (HttpBackend uriText credentials) fetchMode fetchOrder) = do
    backendURL <- maybe
        (fail $ "Invalid backend URL: " ++ show uriText)
        return
        (parseURI $ unpack uriText)
    let backendRequest =
            HTTP.Request
                backendURL
                HTTP.GET
                []
                ""
    response <- HTTP.simpleHTTP backendRequest
    body <- HTTP.getResponseBody response
    headers <- case response of
                    Left err -> fail (show err)
                    Right resp -> return $ HTTP.getHeaders resp
    let mimeType = encodeUtf8 . pack . fromMaybe "text/plain" . lookupHeader HTTP.HdrContentType $ headers
        contentLength = lookupHeader HTTP.HdrContentLength headers >>= readMay
        meta = BackendMeta
                { bmMimeType = mimeType
                , bmMTime = Nothing
                , bmName = pack . takeBaseName . unpack $ uriText
                , bmPath = uriText
                , bmSize = contentLength
                }
    return [BackendSource meta body]

-- | Extract raw integer value from a 'CTime'
unCTime :: CTime -> Int
unCTime (CTime i) = fromIntegral i

-- | Get a HTTP header value by header name from a list of headers.
lookupHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe String
lookupHeader name headers =
    headMay [ v | HTTP.Header n v <- headers, n == name ]

-- | Parse raw backend data source into a structured backend data record.
parseBackendData :: Monad m => BackendSource -> m (BackendData n h)
parseBackendData item@(BackendSource meta body) = do
    let t = takeWhile (/= fromIntegral (ord ';')) (bmMimeType meta)
        parse = fromMaybe parseRawData $ lookup t parsersTable
    parse item

-- | Lookup table of mime types to parsers.
parsersTable :: Monad m => HashMap MimeType (BackendSource -> m (BackendData n h))
parsersTable = mapFromList . mconcat $
    [ zip mimeTypes (repeat parser) | (mimeTypes, parser) <- parsers ]

-- | The parsers we know, by mime types.
parsers :: Monad m => [([MimeType], (BackendSource -> m (BackendData n h)))]
parsers =
    [ ( ["application/json", "text/json"]
      , parseJSONData
      )
    , ( ["application/x-yaml", "text/x-yaml", "application/yaml", "text/yaml"]
      , parseYamlData
      )
    , ( ["application/x-markdown", "text/x-markdown"]
      , parsePandocDataString (Pandoc.readMarkdown Pandoc.def)
      )
    , ( ["application/x-textile", "text/x-textile"]
      , parsePandocDataString (Pandoc.readTextile Pandoc.def)
      )
    , ( ["application/x-rst", "text/x-rst"]
      , parsePandocDataString (Pandoc.readRST Pandoc.def)
      )
    , ( ["application/html", "text/html"]
      , parsePandocDataString (Pandoc.readHtml Pandoc.def)
      )
    , ( ["application/vnd.openxmlformats-officedocument.wordprocessingml.document"]
      , parsePandocDataLBS (fmap fst . Pandoc.readDocx Pandoc.def)
      )
    ]

-- | Parser for raw data (used for static files); this is also the default
-- fallback for otherwise unsupported file types.
parseRawData :: Monad m => BackendSource -> m (BackendData n h)
parseRawData (BackendSource meta body) =
    return $ BackendData
        { bdJSON = JSON.Null
        , bdGVal = toGVal JSON.Null
        , bdMeta = meta
        , bdRaw = body
        }

-- | Parser for JSON source data.
parseJSONData :: Monad m => BackendSource -> m (BackendData n h)
parseJSONData item@(BackendSource meta body) =
    case JSON.eitherDecode body of
        Left err -> fail $ err ++ "\n" ++ show body
        Right json -> return . toBackendData item $ (json :: JSON.Value)

-- | Parser for YAML source data.
parseYamlData :: Monad m => BackendSource -> m (BackendData n h)
parseYamlData item@(BackendSource meta body) =
    case YAML.decodeEither (toStrict body) of
        Left err -> fail $ err ++ "\n" ++ show body
        Right json -> return . toBackendData item $ (json :: JSON.Value)

-- | Parser for Pandoc-supported formats that are read from 'LByteString's.
parsePandocDataLBS :: Monad m
                   => (LByteString -> Either PandocError Pandoc)
                   -> BackendSource
                   -> m (BackendData n h)
parsePandocDataLBS reader input@(BackendSource meta body) = do
    case reader body of
        Left err -> fail . show $ err
        Right pandoc -> return $ toBackendData input pandoc

-- | Parser for Pandoc-supported formats that are read from 'String's.
parsePandocDataString :: Monad m
                   => (String -> Either PandocError Pandoc)
                   -> BackendSource
                   -> m (BackendData n h)
parsePandocDataString reader =
    parsePandocDataLBS (reader . unpack . decodeUtf8)
