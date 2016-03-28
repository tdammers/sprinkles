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
import System.FilePath.Glob (glob)
import System.FilePath
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Network.Wai as Wai
import Network.HTTP.Types (Status, status200, status400, status404, status500)
import qualified Network.Wai.Handler.Warp as Warp
import Data.Default (def)
import Data.ByteString.Builder (stringUtf8)
import System.Directory (makeAbsolute)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP as HTTP
import Network.URI (parseURI)

import Web.Templar.Pattern
import Web.Templar.Replacement

instance FromJSON Pattern where
    parseJSON val = (maybe (fail "invalid pattern") return . parsePattern) =<< parseJSON val

instance FromJSON Replacement where
    parseJSON val = (maybe (fail "invalid replacement") return . parseReplacement) =<< parseJSON val

instance FromJSON ByteString where
    parseJSON val = UTF8.fromString <$> parseJSON val

data Rule =
    Rule
        { ruleRoutePattern :: Pattern
        , ruleBackendPath :: Replacement
        , ruleTemplate :: Text
        }
        deriving (Show, Eq)
$(deriveFromJSON defaultOptions { fieldLabelModifier = drop 4 } ''Rule)

type FrontendPath = [Text]

data ProjectConfig =
    ProjectConfig
        { pcRules :: [Rule]
        , pcBackendBaseURL :: Text
        }

$(deriveFromJSON defaultOptions { fieldLabelModifier = drop 2 } ''ProjectConfig)

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir = do
    YAML.decodeFileEither (dir </> "project.yml") >>=
        either
            (fail . show)
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
        resolver name =
            let name' = makeRelative prefix . normalise $ prefix </> name
            in trace name' $ return $ lookup name' templateSourceMap
    print $ allFilenames
    print . keys $ templateSourceMap
    let relativeFilenames = map (makeRelative prefix) filenames
    templates <- forM relativeFilenames $ \filename -> do
        source <- trace filename $ maybe
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
loadProject dir = do
    config <- loadProjectConfig dir
    templates <- preloadTemplates dir
    return $ Project config templates

resolveTemplateName :: Project -> [Text] -> IO Text
resolveTemplateName project path = do
    let TemplateCache tm = projectTemplates project
    -- return . concat . intersperse "/" $ path
    return "default.html"

resolveTemplate :: Project -> [Text] -> IO Template
resolveTemplate project path = do
    templateName <- resolveTemplateName project path
    getTemplate project templateName

getTemplate :: Project -> Text -> IO Template
getTemplate project templateName = do
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

applyRule :: Rule -> Text -> Maybe (Text, Text)
applyRule rule query = do
    varMap <- matchPattern (ruleRoutePattern rule) query
    return
        ( expandReplacement varMap (ruleBackendPath rule)
        , ruleTemplate rule
        )

applyRules :: [Rule] -> Text -> Maybe (Text, Text)
applyRules [] _ = Nothing
applyRules (rule:rules) query =
    trace (show rule) $
    applyRule rule query <|> applyRules rules query

loadBackendResponse :: String -> IO JSON.Value
loadBackendResponse backendURLStr = do
    backendURL <- maybe
        (fail $ "Invalid backend URL: " ++ backendURLStr)
        return
        (parseURI backendURLStr)
    hPutStrLn stderr backendURLStr
    let backendRequest =
            HTTP.Request
                backendURL
                HTTP.GET
                []
                ""
    backendResponse <- HTTP.simpleHTTP backendRequest
    backendJSON <- HTTP.getResponseBody backendResponse
    hPutStrLn stderr backendJSON
    case JSON.eitherDecode backendJSON of
        Left err -> fail $ err ++ "\n" ++ show backendJSON
        Right json -> return (json :: JSON.Value)

respondTemplate :: Project -> Status -> Text -> HashMap Text (GVal (Ginger.Run IO)) -> Wai.Application
respondTemplate project status templateName contextMap request respond = do
    let contextMap' =
            contextMap <>
            mapFromList
                [ "request" ~> request
                ]
        contextLookup key = return . fromMaybe def $ lookup key contextMap
        headers = [("Content-type", "text/html;charset=utf8")]
    template <- getTemplate project templateName
    respond . Wai.responseStream status200 headers $ \write flush -> do
        let writeHtml = write . stringUtf8 . unpack . htmlSource
            context :: GingerContext IO
            context = Ginger.makeContextM contextLookup writeHtml
        runGingerT context template
        flush

handleRequest :: Project -> Wai.Application
handleRequest project request respond = do
    go `catch` serve500
    where
        serve500 (err :: SomeException) =
            respondTemplate
                project
                status500
                "500.html"
                (mapFromList [])
                request
                respond
        go = do
            let backendBaseURL = pcBackendBaseURL (projectConfig project)
                queryPath =
                    (pack . UTF8.toString $ Wai.rawPathInfo request) <>
                    (pack . UTF8.toString $ Wai.rawQueryString request)
            case applyRules (pcRules . projectConfig $ project) queryPath of
                Nothing ->
                    respondTemplate
                        project
                        status404
                        "404.html"
                        (mapFromList [])
                        request
                        respond
                Just (backendPath, templateName) -> do
                    let backendURLStr :: String
                        backendURLStr = unpack $ backendBaseURL <> backendPath
                    backendData <- loadBackendResponse backendURLStr
                    respondTemplate
                        project
                        status200
                        templateName
                        (mapFromList [ "data" ~> backendData ])
                        request
                        respond
