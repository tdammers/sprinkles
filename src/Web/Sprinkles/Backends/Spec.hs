{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE TypeApplications #-}

-- | Backend spec types and parser
module Web.Sprinkles.Backends.Spec
(
-- * Defining backends
  BackendSpec (..)
, backendSpecFromJSON
, makeBackendTypePathsAbsolute
, makeBackendSpecPathsAbsolute
, BackendType (..)
, FetchOrderField (..)
, FetchMode (..)
, AscDesc (..)
, FetchOrder (..)
, parseBackendURI
, Credentials (..)
, HttpMethod (..)
, HttpBackendOptions (..)
, CachePolicy (..)
, HasCachePolicy (..)
, ParserType (..)
, parserTypes
)
where

import Web.Sprinkles.Prelude
import Network.Mime (MimeType)
import Network.HTTP.Types ()
import Data.Aeson (FromJSON (..), Value (..), (.=), (.!=), (.:?), (.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import System.PosixCompat.Files
import Data.Default (Default (..))
import Web.Sprinkles.Cache
import qualified Data.Serialize as Cereal
import Data.Serialize (Serialize)
import Web.Sprinkles.Databases (DSN (..), sqlDriverFromID, ResultSetMode (..))
import Web.Sprinkles.Logger (LogLevel (..))
import Data.Expandable (ExpandableM (..), expand)
import GHC.Stack

-- | A type of backend.
data BackendType = HttpBackend Text HttpBackendOptions -- ^ Fetch data over HTTP(S)
                 | FileBackend Text -- ^ Read local files
                 | SqlBackend DSN Text [Text] -- ^ Query an SQL database
                 | SqlMultiBackend DSN ResultSetMode [(Text, [Text])] -- ^ Query an SQL database, multiple queries
                 | SubprocessBackend Text [Text] MimeType -- ^ Run a command in a subprocess
                 | RequestBodyBackend -- ^ Read the incoming request body
                 | LiteralBackend Value -- ^ Return literal data from the spec itself
                 deriving (Show, Generic)

makeBackendTypePathsAbsolute :: FilePath -> BackendType -> BackendType
makeBackendTypePathsAbsolute dir (FileBackend fn) = FileBackend (pack . (dir </>) . unpack $ fn)
makeBackendTypePathsAbsolute dir (SubprocessBackend cmd args ty) = SubprocessBackend (pack . (dir </>) . unpack $ cmd) args ty
makeBackendTypePathsAbsolute _ x = x

instance Serialize BackendType where
    put (HttpBackend url options) = do
        Cereal.put 'h'
        Cereal.put (encodeUtf8 url)
        Cereal.put options
    put (FileBackend path) = do
        Cereal.put 'f'
        Cereal.put (encodeUtf8 path)
    put (SqlBackend dsn query params) = do
        Cereal.put 's'
        Cereal.put dsn
        Cereal.put (encodeUtf8 query)
        Cereal.put (map encodeUtf8 params)
    put (SqlMultiBackend dsn mode queries) = do
        Cereal.put 'S'
        Cereal.put dsn
        Cereal.put mode
        Cereal.put
            [ (encodeUtf8 $ q, map encodeUtf8 p)
            | (q, p) <- queries
            ]
    put (SubprocessBackend cmd args t) = do
        Cereal.put 'p'
        Cereal.put (encodeUtf8 cmd)
        Cereal.put (map encodeUtf8 args)
        Cereal.put t
    put RequestBodyBackend = Cereal.put 'b'
    put (LiteralBackend b) = do
        Cereal.put 'l'
        Cereal.put (JSON.encode b)
    get = Cereal.get >>= \case
            'h' -> HttpBackend <$> (decodeUtf8 <$> Cereal.get) <*> Cereal.get
            'f' -> FileBackend <$> (decodeUtf8 <$> Cereal.get)
            's' -> SqlBackend <$>
                    Cereal.get <*>
                    (decodeUtf8 <$> Cereal.get) <*>
                    (map decodeUtf8 <$> Cereal.get)
            'S' -> SqlMultiBackend <$>
                    Cereal.get <*>
                    Cereal.get <*>
                    (Cereal.get >>= \items -> return
                        [ ( decodeUtf8 q
                          , map decodeUtf8 p
                          )
                        | (q, p) <- items
                        ])
            'p' -> SubprocessBackend <$>
                    (decodeUtf8 <$> Cereal.get) <*>
                    (map decodeUtf8 <$> Cereal.get) <*>
                    Cereal.get
            'b' -> return RequestBodyBackend
            'l' -> LiteralBackend <$>
                    (fromMaybe JSON.Null . JSON.decode <$> Cereal.get)
            x -> fail $ "Invalid backend type identifier: " <> show x

instance ExpandableM Text BackendType where
    expandM f (HttpBackend t c) =
        HttpBackend <$> f t <*> pure c
    expandM f (FileBackend t) =
        FileBackend <$> f t
    expandM f (SqlBackend dsn query params) =
        SqlBackend <$> expandM f dsn <*> pure query <*> expandM f params
    expandM f (SqlMultiBackend dsn mode queries) =
        SqlMultiBackend
            <$> expandM f dsn
            <*> pure mode
            <*> ( forM queries $ \(query, params) ->
                    (query,) <$> expandM f params
                )
    expandM f (SubprocessBackend cmd args t) =
        SubprocessBackend cmd <$> expandM f args <*> pure t
    expandM _ RequestBodyBackend =
        pure RequestBodyBackend
    expandM f (LiteralBackend b) =
        LiteralBackend <$> expandM f b

-- | A specification of a backend query.
data BackendSpec =
    BackendSpec
        { bsType :: BackendType -- ^ Defines the data source
        , bsFetchMode :: FetchMode -- ^ How many items to fetch, and in what shape
        , bsOrder :: FetchOrder -- ^ How to order items
        -- | If set, ignore reported MIME type and use this one instead.
        , bsMimeTypeOverride :: Maybe MimeType
        , bsCacheEnabled :: Bool
        }
        deriving (Show, Generic)

instance Serialize BackendSpec

makeBackendSpecPathsAbsolute :: FilePath -> BackendSpec -> BackendSpec
makeBackendSpecPathsAbsolute dir spec =
  spec { bsType = makeBackendTypePathsAbsolute dir (bsType spec) }

instance ExpandableM Text BackendSpec where
    expandM f (BackendSpec t m o mto ce) =
        BackendSpec <$> expandM f t
                    <*> pure m
                    <*> pure o
                    <*> pure mto
                    <*> pure ce

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
--   // - "subprocess" (execute a subprocess and read its stdout)
--   // - "post" (get the request body; only for POST requests)
--   // - "literal" (return literal value as specified)
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
backendSpecFromJSON :: HasCallStack => JSON.Value -> JSON.Parser BackendSpec
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
            "post" -> return (RequestBodyBackend, FetchOne)
            "literal" -> parseLiteralBackendSpec
            x -> fail $ "Invalid backend specifier: " ++ show x
    fetchMode <- obj .:? "fetch" .!= defFetchMode
    fetchOrder <- obj .:? "order" .!= def
    cacheEnabled <- obj .:? "cache-enabled" .!= True
    mimeOverride <- fmap encodeUtf8 . (id @(Maybe Text)) <$> obj .:? "force-mime-type"
    return $ BackendSpec t fetchMode fetchOrder mimeOverride cacheEnabled
    where
        parseHttpBackendSpec = do
            t <- obj .: "uri"
            return (HttpBackend t def, FetchOne)
        parseFileBackendSpec m = do
            path <- obj .: "path"
            return (FileBackend path, m)
        parseDirBackendSpec = do
            path <- obj .: "path"
            return (FileBackend (pack $ path </> "*"), FetchAll)
        parseSqlBackendSpec = do
            dsn <- obj .: "connection"
            case lookup "queries" obj of
                Nothing -> do
                    query <- obj .: "query"
                    params <- obj .:? "params" .!= []
                    return (SqlBackend dsn query params, FetchAll)
                Just (Array queries') -> do
                    queries <- forM (toList queries') $ \case
                        String queryStr -> do
                            return (queryStr, [])
                        Object queriesObj -> do
                            query <- queriesObj .: "query"
                            params <- queriesObj .:? "params" .!= []
                            return (query, params)
                        Array queriesArr -> do
                            case toList queriesArr of
                                [] ->
                                    fail "Invalid query object, empty array is not allowed"
                                [String queryStr] ->
                                    return (queryStr, [])
                                [String queryStr, Array params] ->
                                    (queryStr,) <$> mapM parseJSON (toList params)
                                (String queryStr:params) ->
                                    (queryStr,) <$> mapM parseJSON params
                                x ->
                                    fail "Invalid query object, first array element must be string"
                        x -> fail "Invalid query object, must be array, string, or object"
                    mode <- obj .:? "results" .!= ResultsMerge
                    return (SqlMultiBackend dsn mode queries, FetchAll)
                Just x -> fail "Invalid queries object, must be array"
        parseSubprocessSpec = do
            rawCmd <- obj .: "cmd"
            t <- fromString <$> (obj .:? "mime-type" .!= "text/plain")
            case rawCmd of
                String cmd -> return (SubprocessBackend cmd [] t, FetchOne)
                Array v -> parseJSON rawCmd >>= \case
                    cmd:args -> return (SubprocessBackend cmd args t, FetchOne)
                    _ -> fail "Expected a command and a list of arguments"
                x -> fail $ "Expected string or array, but found " ++ show x
        parseLiteralBackendSpec = do
            b <- obj .:? "body" .!= JSON.Null
            return (LiteralBackend b, FetchOne)

backendSpecFromJSON x = fail $ "Invalid JSON value for BackendSpec: " <> show x <> ", expecting object or string"

-- | Parse a 'Text' into a 'BackendSpec'.
parseBackendURI :: MonadFail m => Text -> m BackendSpec
parseBackendURI t = do
    let protocol = takeWhile (/= ':') t
        path = drop (length protocol + 3) t
    case protocol of
        "http" ->
            return $
                BackendSpec
                    (HttpBackend t def)
                    FetchOne
                    def
                    Nothing
                    True
        "https" ->
            return $
                BackendSpec
                    (HttpBackend t def)
                    FetchOne
                    def
                    Nothing
                    True
        "dir" -> return $
            BackendSpec (FileBackend (pack $ unpack path </> "*")) FetchAll def Nothing True
        "glob" -> return $
            BackendSpec (FileBackend path) FetchAll def Nothing True
        "file" -> return $
            BackendSpec (FileBackend path) FetchOne def Nothing True
        "sql" -> do
            be <- parseSqlBackendURI path
            return $ BackendSpec be FetchAll def Nothing True
        "post" ->
            return $
                BackendSpec
                    RequestBodyBackend
                    FetchOne def Nothing True
        "literal" ->
            return $
                BackendSpec
                    (LiteralBackend $ JSON.String path)
                    FetchOne def Nothing True
        _ -> fail $ "Unknown protocol: " <> show protocol
    where
        parseSqlBackendURI path = do
            let driverID = takeWhile (/= ':') path
                remainder = drop (length driverID + 1) path
                details = takeWhile (/= ':') remainder
                query = drop (length details + 1) remainder
            driver <- either
                (\msg ->
                    fail $ "Invalid driver: " ++ show driverID ++ "(" ++ msg ++ ")")
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

data HttpMethod = GET | POST
    deriving (Show, Generic)

instance Serialize HttpMethod where

data HttpBackendOptions =
    HttpBackendOptions
        { httpCredentials :: Credentials
        , httpHttpMethods :: HttpMethod
        , httpAcceptedContentTypes :: [MimeType]
        }
        deriving (Show, Generic)

instance Serialize HttpBackendOptions where

instance FromJSON Credentials where
    parseJSON Null = return AnonymousCredentials
    parseJSON (String "anonymous") = return AnonymousCredentials
    parseJSON _ = fail "Invalid credentials"

instance FromJSON HttpMethod where
    parseJSON (String str) =
        case toUpper str of
            "GET" -> return GET
            "POST" -> return POST
            x -> fail $ "Unsupported request method: " <> show x
    parseJSON _ = fail "Invalid request method, expected string"

instance FromJSON HttpBackendOptions where
    parseJSON Null = return def
    parseJSON (Object o) =
        HttpBackendOptions
            <$> (o .:? "credentials" .!= AnonymousCredentials)
            <*> (o .:? "method" .!= GET)
            <*> pure knownContentTypes
    parseJSON x =
        fail $ "Expected string or array, but found " ++ show x

instance Default HttpBackendOptions where
    def = HttpBackendOptions
            AnonymousCredentials
            GET
            knownContentTypes

data CachePolicy = CacheForever
                 | NoCaching

class HasCachePolicy a where
    cachePolicy :: a -> CachePolicy

instance HasCachePolicy BackendSpec where
    cachePolicy = cachePolicy . bsType

instance HasCachePolicy BackendType where
    cachePolicy = \case
        RequestBodyBackend -> NoCaching
        _ -> CacheForever

data ParserType = ParserText
                | ParserJSON
                | ParserYAML
                | ParserFormUrlencoded
                | ParserMarkdown
                | ParserCreole
                | ParserTextile
                | ParserRST
                | ParserLaTeX
                | ParserDocX
                | ParserHtml
                deriving (Show, Read)

-- | The parsers we know, by mime types.
parserTypes :: [([MimeType], ParserType)]
parserTypes =
    [ ( [ "application/json", "text/json" ]
      , ParserJSON
      )
    , ( [ "application/x-yaml"
        , "text/x-yaml"
        , "application/yaml"
        , "text/yaml"
        ]
      , ParserYAML
      )
    , ( [ "application/x-www-form-urlencoded"
        ]
      , ParserFormUrlencoded
      )
    , ( [ "application/x-markdown"
        , "text/x-markdown"
        ]
      , ParserMarkdown
      )
    , ( [ "application/x-creole"
        , "text/x-creole"
        ]
      , ParserCreole
      )
    , ( [ "application/x-textile"
        , "text/x-textile"
        ]
      , ParserTextile
      )
    , ( [ "application/x-rst"
        , "text/x-rst"
        ]
      , ParserRST
      )
    , ( [ "application/x-latex"
        , "text/x-latex"
        , "application/x-tex"
        , "text/x-tex"
        ]
      , ParserLaTeX
      )
    , ( [ "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        ]
      , ParserDocX
      )
    , ( [ "text/plain" ]
      , ParserText
      )
    , ( [ "application/html"
        , "text/html"
        ]
      , ParserHtml
      )
    ]

-- | All the content types we know how to parse
knownContentTypes :: [MimeType]
knownContentTypes = concatMap fst parserTypes
