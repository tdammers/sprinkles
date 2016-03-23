{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar
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
import System.FilePath.Glob
import System.FilePath
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import Network.HTTP.Types (status200, status500)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Default (def)
import Data.ByteString.Builder (stringUtf8)
import System.Directory (makeAbsolute)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP as HTTP

data RoutePatternItem =
    LiteralComponent Text |
    NamedComponent Text |
    AnyComponent
    deriving (Ord, Eq, Show, Read)

$(deriveJSON defaultOptions ''RoutePatternItem)

newtype RoutePattern = RoutePattern [RoutePatternItem]
    deriving (Ord, Eq, Show, Read, ToJSON, FromJSON)

data TemplateRule =
    TemplateRule
        { trRoutePattern :: RoutePattern
        , trTemplate :: Text
        }
$(deriveJSON defaultOptions { fieldLabelModifier = drop 2 } ''TemplateRule)

type FrontendPath = [Text]

data ProjectConfig =
    ProjectConfig
        { pcTemplateRules :: [TemplateRule]
        , pcBackendBaseURL :: Text
        }

$(deriveJSON defaultOptions { fieldLabelModifier = drop 2 } ''ProjectConfig)

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir = do
    YAML.decodeFile (dir </> "project.yml") >>=
        maybe
            (fail "Invalid YAML in project.yml, or project.yml not found")
            return

newtype TemplateCache = TemplateCache (HashMap Text Template)

preloadTemplates :: FilePath -> IO TemplateCache
preloadTemplates dir = do
    prefix <- makeAbsolute $ dir </> "templates"
    allFilenames <- glob $ prefix </> "**" </> "*.html"
    filenames <- glob $ prefix </> "*.html"
    templateSources <- forM allFilenames readFile
    let templateSourceMap :: HashMap String String
        templateSourceMap =
            mapFromList $
                zip
                    (map (makeRelative prefix) allFilenames)
                    templateSources
        resolver :: String -> IO (Maybe String)
        resolver name = return $ lookup name templateSourceMap
    print . keys $ templateSourceMap
    let relativeFilenames = map (makeRelative prefix) filenames
    templates <- forM relativeFilenames $ \filename -> do
        source <- maybe
                    (fail $ "Source not found: " <> filename)
                    return
                    (lookup filename templateSourceMap)
        parseGinger resolver (Just filename) source >>= \case
            Left err -> fail . show $ err
            Right t -> return t
    return . TemplateCache . mapFromList $ zip (map pack relativeFilenames) templates

data Project =
    Project
        { projectConfig :: ProjectConfig
        , projectTemplates :: TemplateCache
        }

loadProject :: FilePath -> IO Project
loadProject dir =
    Project <$> loadProjectConfig dir <*> preloadTemplates dir

resolveTemplateName :: Project -> [Text] -> IO Text
resolveTemplateName project path = do
    let TemplateCache tm = projectTemplates project
    -- return . concat . intersperse "/" $ path
    return "default.html"

resolveTemplate :: Project -> [Text] -> IO Template
resolveTemplate project path = do
    templateName <- resolveTemplateName project path
    let TemplateCache tm = projectTemplates project
    maybe
        (fail $ "Template not found: " <> unpack templateName)
        return
        (lookup templateName tm)

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

handleRequest :: Project -> Wai.Application
handleRequest project request respond = do
    let backendBaseURL = pcBackendBaseURL (projectConfig project)
        backendURL =
            (unpack backendBaseURL) <>
            (UTF8.toString $ Wai.rawPathInfo request) <>
            (UTF8.toString $ Wai.rawQueryString request)
    backendResponse <- HTTP.simpleHTTP (HTTP.getRequest backendURL)
    backendJSON <- HTTP.getResponseBody backendResponse
    hPutStrLn stderr backendURL
    backendData <- case JSON.eitherDecode . LUTF8.fromString $ backendJSON of
        Left err -> fail err
        Right json -> return (json :: JSON.Value)

    respond . Wai.responseStream status200 [] $ \write flush -> do
        template <- resolveTemplate project (Wai.pathInfo request)
        let contextMap :: HashMap Text (GVal (Ginger.Run IO))
            contextMap =
                mapFromList
                    [ "request" ~> request
                    , "data" ~> backendData
                    ]
            contextLookup key = return . fromMaybe def $ lookup key contextMap
            writeHtml = write . stringUtf8 . unpack . htmlSource
            context :: GingerContext IO
            context = Ginger.makeContextM contextLookup writeHtml
        runGingerT context template
