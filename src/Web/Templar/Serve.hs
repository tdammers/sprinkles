{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE PartialTypeSignatures #-}
module Web.Templar.Serve
( serveProject
)
where

import ClassyPrelude
import Text.Ginger
        ( parseGinger
        , Template
        , runGingerT
        , GingerContext
        , GVal (..)
        , ToGVal (..)
        , (~>)
        )
import Text.Ginger.Html (Html, htmlSource)
import qualified Text.Ginger as Ginger
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Yaml as YAML
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import Network.HTTP.Types (Status, status200, status302, status400, status404, status500)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Wai.Handler.SCGI as SCGI
import qualified Network.Wai.Handler.FastCGI as FastCGI
import Data.Default (def)
import Data.ByteString.Builder (stringUtf8)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.CaseInsensitive as CI
import Control.Concurrent (forkIO, threadDelay)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Readers.Creole as Pandoc

import Web.Templar.Backends
import Web.Templar.Rule
import Web.Templar.ProjectConfig
import Web.Templar.Cache
import Web.Templar.ServerConfig
import Web.Templar.Project
import Web.Templar.Logger as Logger

serveProject :: ServerConfig -> Project -> IO ()
serveProject config project = do
    forkIO vacuum
    serve project
    where
        serve = case scDriver config of
            DefaultDriver -> serveWarp 5000
            WarpDriver port -> serveWarp port
            CGIDriver -> serveCGI
            SCGIDriver -> serveSCGI
            FastCGIDriver -> serveFastCGI
        vacuum = forever $ do
            itemsCleared <- cacheVacuum (projectBackendCache project)
            when (itemsCleared > 0) $ do
                writeLog (projectLogger project) Notice $
                    "Cache items deleted: " <> tshow itemsCleared
            threadDelay 5000000 -- check every 5 seconds

serveWarp :: Int -> Project -> IO ()
serveWarp port project = do
    writeLog (projectLogger project) Notice $
        "Running server on port " <> tshow port <> "..."
    Warp.run port (appFromProject project)

serveCGI :: Project -> IO ()
serveCGI project = CGI.run (appFromProject project)

serveSCGI :: Project -> IO ()
serveSCGI project = SCGI.run (appFromProject project)

serveFastCGI :: Project -> IO ()
serveFastCGI project = FastCGI.run (appFromProject project)

instance ToGVal m ByteString where
    toGVal = toGVal . UTF8.toString

instance ToGVal m (CI.CI ByteString) where
    toGVal = toGVal . CI.original

instance ToGVal m Wai.Request where
    toGVal rq =
        Ginger.dict
            [ "method" ~> show (Wai.requestMethod rq)
            , "httpVersion" ~> show (Wai.httpVersion rq)
            , "headers" ~> Wai.requestHeaders rq
            , "pathInfo" ~> Wai.pathInfo rq
            , "path" ~> Wai.rawPathInfo rq
            , "query" ~> Wai.rawQueryString rq
            ]

appFromProject :: Project -> Wai.Application
appFromProject project request respond = do
    handleRequest project request respond `catch` handleException
    where
        handleException (e :: SomeException) = do
            writeLog (projectLogger project) Logger.Error $ tshow e
            respond $ Wai.responseLBS status500 [] "Something went pear-shaped."

respondTemplateHtml :: ToGVal (Ginger.Run IO Html) a => Project -> Status -> Text -> HashMap Text a -> Wai.Application
respondTemplateHtml project status templateName contextMap request respond = do
    let contextLookup = mkContextLookup request project contextMap
        headers = [("Content-type", "text/html;charset=utf8")]
    template <- getTemplate project templateName
    respond . Wai.responseStream status200 headers $ \write flush -> do
        let writeHtml = write . stringUtf8 . unpack . htmlSource
            context :: GingerContext IO Html
            context = Ginger.makeContextHtmlM contextLookup writeHtml
        runGingerT context template
        flush

respondTemplateText :: ToGVal (Ginger.Run IO Text) a => Project -> Status -> Text -> HashMap Text a -> Wai.Application
respondTemplateText project status templateName contextMap request respond = do
    let contextLookup = mkContextLookup request project contextMap
        headers = [("Content-type", "text/plain;charset=utf8")]
    template <- getTemplate project templateName
    respond . Wai.responseStream status200 headers $ \write flush -> do
        let writeText = write . stringUtf8 . unpack
            context :: GingerContext IO Text
            context = Ginger.makeContextTextM contextLookup writeText
        runGingerT context template
        flush

mkContextLookup :: (ToGVal (Ginger.Run IO h) a)
                => Wai.Request
                -> Project
                -> HashMap Text a
                -> Text
                -> Ginger.Run IO h (GVal (Ginger.Run IO h))
mkContextLookup request project contextMap key = do
    let cache = projectBackendCache project
        logger = projectLogger project
        contextMap' =
            fmap toGVal contextMap <>
            mapFromList
                [ "request" ~> request
                , ("load", Ginger.fromFunction (gfnLoadBackendData (writeLog logger) cache))
                , ("ellipse", Ginger.fromFunction gfnEllipse)
                , ("json", Ginger.fromFunction gfnJSON)
                , ("yaml", Ginger.fromFunction gfnYAML)
                , ("pandoc", Ginger.fromFunction (gfnPandoc (writeLog logger)))
                ]
    return . fromMaybe def $ lookup key contextMap'

gfnLoadBackendData :: forall h. (LogLevel -> Text -> IO ()) -> RawBackendCache -> Ginger.Function (Ginger.Run IO h)
gfnLoadBackendData writeLog cache args =
    Ginger.dict <$> forM (zip [0..] args) loadPair
    where
        loadPair :: (Int, (Maybe Text, GVal (Ginger.Run IO h)))
                 -> Ginger.Run IO h (Text, GVal (Ginger.Run IO h))
        loadPair (index, (keyMay, gBackendURL)) = do
            let backendURL = Ginger.asText $ gBackendURL
            backendData :: Items (BackendData IO h) <- liftIO $
                loadBackendData writeLog cache =<< parseBackendURI backendURL
            return
                ( fromMaybe (tshow index) keyMay
                , toGVal backendData
                )

gfnPandoc :: forall h. (LogLevel -> Text -> IO ()) -> Ginger.Function (Ginger.Run IO h)
gfnPandoc writeLog [(Nothing, src), (Nothing, readerName)] = liftIO $
    (toGVal <$> pandoc (unpack $ Ginger.asText readerName) (Ginger.asText src))
    `catch` (\(e :: SomeException) -> do
        writeLog Logger.Error . tshow $ e
        return . toGVal $ False
    )

pandoc :: String -> Text -> IO Pandoc.Pandoc
pandoc readerName src = do
    reader <- either
        (\err -> fail $ "Invalid reader: " ++ show err)
        return
        (getReader readerName)
    let read = case reader of
            Pandoc.StringReader r -> r Pandoc.def . unpack
            Pandoc.ByteStringReader r -> fmap (fmap fst) . r Pandoc.def . encodeUtf8
    read (fromStrict src) >>= either
        (\err -> fail $ "Reading " ++ show readerName ++ " failed: " ++ show err)
        return
    where
        getReader "creole" = Right $ Pandoc.mkStringReader Pandoc.readCreole
        getReader readerName = Pandoc.getReader readerName

gfnEllipse :: Ginger.Function (Ginger.Run IO h)
gfnEllipse [] = return def
gfnEllipse [(Nothing, str)] =
    gfnEllipse [(Nothing, str), (Nothing, toGVal (100 :: Int))]
gfnEllipse [(Nothing, str), (Nothing, len)] = do
    let txt = Ginger.asText str
        actualLen = ClassyPrelude.length txt
        targetLen = fromMaybe 100 $ ceiling <$> Ginger.asNumber len
        txt' = if actualLen + 3 > targetLen
                    then take (targetLen - 3) txt <> "..."
                    else txt
    return . toGVal $ txt'
gfnEllipse ((Nothing, str):xs) = do
    let len = fromMaybe (toGVal (100 :: Int)) $ lookup (Just "len") xs
    gfnEllipse [(Nothing, str), (Nothing, len)]
gfnEllipse xs = do
    let str = fromMaybe def $ lookup (Just "str") xs
    gfnEllipse $ (Nothing, str):xs

gfnJSON :: Ginger.Function (Ginger.Run IO h)
gfnJSON [] = return def
gfnJSON ((_, x):xs) = do
    return . toGVal . LUTF8.toString . JSON.encodePretty $ x

gfnYAML :: Ginger.Function (Ginger.Run IO h)
gfnYAML [] = return def
gfnYAML ((_, x):xs) = do
    return . toGVal . UTF8.toString . YAML.encode $ x

data NotFoundException = NotFoundException
    deriving (Show)

instance Exception NotFoundException where

handleRequest :: Project -> Wai.Application
handleRequest project request respond = do
    go `catchIOError` \e -> handle500 e project request respond
    where
        cache = projectBackendCache project
        go = do
            let queryPath =
                    (pack . UTF8.toString $ Wai.rawPathInfo request) <>
                    (pack . UTF8.toString $ Wai.rawQueryString request)
            let globalBackendPaths = pcContextData . projectConfig $ project
            case applyRules (pcRules . projectConfig $ project) queryPath of
                Nothing ->
                    handle404
                        (globalBackendPaths, setFromList [])
                        project
                        request
                        respond
                Just (backendPaths, required, target) -> do
                    let handle = case target of
                            RedirectTarget redirectPath ->
                                handleRedirectTarget
                                    redirectPath

                            StaticTarget ->
                                handleStaticTarget

                            JSONTarget ->
                                handleJSONTarget

                            TemplateTarget templateName ->
                                handleTemplateTarget
                                    templateName
                    handle
                        (globalBackendPaths <> backendPaths, required)
                        project
                        request
                        respond

handle404 :: (HashMap Text BackendSpec, Set Text)
          -> Project
          -> Wai.Application
handle404 (backendPaths, required) project request respond = do
    let cache = projectBackendCache project
        logger = projectLogger project
    backendData <- loadBackendDict (writeLog logger) cache backendPaths required
    respondTemplateHtml
        project
        status404
        "404.html"
        backendData
        request
        respond

handle500 :: Show e
          => e
          -> Project
          -> Wai.Application
handle500 err project request respond = do
    writeLog (projectLogger project) Logger.Error $ tshow err
    let cache = projectBackendCache project
        backendPaths = pcContextData . projectConfig $ project
        logger = projectLogger project
    backendData <- loadBackendDict (writeLog logger) cache backendPaths (setFromList [])
    respondTemplateHtml
        project
        status500
        "500.html"
        backendData
        request
        respond

handleRedirectTarget :: Text
                     -> (HashMap Text BackendSpec, Set Text)
                     -> Project
                     -> Wai.Application
handleRedirectTarget redirectPath
                     (backendPaths, required) 
                     project
                     request
                     respond = do
    respond $ Wai.responseLBS
        status302
        [("Location", UTF8.fromString . unpack $ redirectPath)]
        ""

handleJSONTarget :: (HashMap Text BackendSpec, Set Text)
                 -> Project
                 -> Wai.Application
handleJSONTarget (backendPaths, required) 
                 project
                 request
                 respond = do
    let cache = projectBackendCache project
        logger = projectLogger project
    backendData <- loadBackendDict (writeLog logger) cache backendPaths required
    respond $ Wai.responseLBS
        status200
        [("Content-type", "application/json")]
        (JSON.encode backendData)

handleTemplateTarget :: Text
                     -> (HashMap Text BackendSpec, Set Text)
                     -> Project
                     -> Wai.Application
handleTemplateTarget templateName
                     (backendPaths, required)
                     project
                     request
                     respond = do
    let cache = projectBackendCache project
        logger = projectLogger project
        go = do
            backendData <- loadBackendDict (writeLog logger) cache backendPaths required
            respondTemplateHtml
                project
                status200
                templateName
                backendData
                request
                respond
    go `catch` handleNotFound project request respond

handleStaticTarget :: (HashMap Text BackendSpec, Set Text)
                     -> Project
                     -> Wai.Application
handleStaticTarget (backendPaths, required)
                   project
                   request
                   respond = do
    let cache = projectBackendCache project
        logger = projectLogger project
        go = do
            backendData <- loadBackendDict (writeLog logger) cache backendPaths required
            backendItem <- case lookup "file" backendData of
                Nothing -> throwM NotFoundException
                Just NotFound -> throwM NotFoundException
                Just (SingleItem item) -> return item
                Just (MultiItem []) -> throwM NotFoundException
                Just (MultiItem (x:xs)) -> return x
            respond $ Wai.responseLBS
                status200
                [("Content-type", bmMimeType . bdMeta $ backendItem)]
                (bdRaw $ backendItem)
    go `catch` handleNotFound project request respond

handleNotFound :: Project -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> NotFoundException -> IO Wai.ResponseReceived
handleNotFound project request respond _ = do
    let globalBackendPaths = pcContextData . projectConfig $ project
    handle404
        (globalBackendPaths, setFromList [])
        project
        request
        respond

loadBackendDict :: (LogLevel -> Text -> IO ()) -> RawBackendCache -> HashMap Text BackendSpec -> Set Text -> IO (HashMap Text (Items (BackendData IO Html)))
loadBackendDict writeLog cache backendPaths required = do
    pairs <- forM (mapToList backendPaths) $ \(key, backendPath) -> do
        bd :: Items (BackendData IO Html) <- loadBackendData writeLog cache backendPath
        case bd of
            NotFound ->
                if key `elem` required
                    then throwM NotFoundException
                    else return $ (key, NotFound)
            _ -> return (key, bd)
    return $ mapFromList pairs

