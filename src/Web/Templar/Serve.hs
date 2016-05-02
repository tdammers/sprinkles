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
import Data.Yaml as YAML
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import Network.HTTP.Types (Status, status200, status302, status400, status404, status500)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Default (def)
import Data.ByteString.Builder (stringUtf8)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.CaseInsensitive as CI

import Web.Templar.Backends
import Web.Templar.Rule
import Web.Templar.ProjectConfig
import Web.Templar.Project

serveProject :: Project -> Int -> IO ()
serveProject project port =
    Warp.run port (appFromProject project)

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
            hPutStrLn stderr $ show e
            respond $ Wai.responseLBS status500 [] "Something went pear-shaped."

respondTemplateHtml :: ToGVal (Ginger.Run IO Html) a => Project -> Status -> Text -> HashMap Text a -> Wai.Application
respondTemplateHtml project status templateName contextMap request respond = do
    let contextLookup = mkContextLookup request contextMap
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
    let contextLookup = mkContextLookup request contextMap
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
                -> HashMap Text a
                -> Text
                -> Ginger.Run IO h (GVal (Ginger.Run IO h))
mkContextLookup request contextMap key = do
    let contextMap' =
            fmap toGVal contextMap <>
            mapFromList
                [ "request" ~> request
                , ("load", Ginger.fromFunction gfnLoadBackendData)
                , ("ellipse", Ginger.fromFunction gfnEllipse)
                ]
    return . fromMaybe def $ lookup key contextMap'

gfnLoadBackendData :: forall h. Ginger.Function (Ginger.Run IO h)
gfnLoadBackendData args =
    Ginger.dict <$> forM (zip [0..] args) loadPair
    where
        loadPair :: (Int, (Maybe Text, GVal (Ginger.Run IO h)))
                 -> Ginger.Run IO h (Text, GVal (Ginger.Run IO h))
        loadPair (index, (keyMay, gBackendURL)) = do
            let backendURL = Ginger.asText $ gBackendURL
            backendData :: Items (BackendData IO h) <- liftIO $
                loadBackendData =<< parseBackendURI backendURL
            return
                ( fromMaybe (tshow index) keyMay
                , toGVal backendData
                )

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


data NotFoundException = NotFoundException
    deriving (Show)

instance Exception NotFoundException where

handleRequest :: Project -> Wai.Application
handleRequest project request respond = do
    go `catchIOError` \e -> handle500 e project request respond
    where
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
    backendData <- loadBackendDict backendPaths required
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
    hPutStrLn stderr . show $ err
    let backendPaths = pcContextData . projectConfig $ project
    backendData <- loadBackendDict backendPaths (setFromList [])
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
    backendData <- loadBackendDict backendPaths required
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
    let go = do
            backendData <- loadBackendDict backendPaths required
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
    let go = do
            backendData <- loadBackendDict backendPaths required
            backendItem <- case lookup "file" backendData of
                Nothing -> throwM NotFoundException
                Just NotFound -> throwM NotFoundException
                Just (SingleItem item) -> return item
                Just (MultiItem []) -> throwM NotFoundException
                Just (MultiItem (x:_)) -> return x
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

loadBackendDict :: HashMap Text BackendSpec -> Set Text -> IO (HashMap Text (Items (BackendData IO Html)))
loadBackendDict backendPaths required = do
    pairs <- forM (mapToList backendPaths) $ \(key, backendPath) -> do
        bd :: Items (BackendData IO Html) <- loadBackendData backendPath
        case bd of
            NotFound ->
                if key `elem` required
                    then throwM NotFoundException
                    else return $ (key, NotFound)
            _ -> return (key, bd)
    return $ mapFromList pairs

