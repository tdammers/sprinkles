{-#LANGUAGE DeriveGeneric #-}
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
module Web.Templar.Serve
( serveProject
)
where

import ClassyPrelude
import Control.Concurrent (forkIO)
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.TH as JSON
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.Default (def)
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Yaml as YAML

import Network.HTTP.Types
       (Status, status200, status302, status400, status404, status500)
import Network.HTTP.Types.URI (queryToQueryText)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.CGI as CGI
import qualified Network.Wai.Handler.FastCGI as FastCGI
import qualified Network.Wai.Handler.SCGI as SCGI
import qualified Network.Wai.Handler.Warp as Warp

import System.Environment (lookupEnv)
import System.Locale.Read (getLocale)
import qualified Text.Ginger as Ginger
import Text.Ginger
       (parseGinger, Template, runGingerT, GingerContext, GVal(..), ToGVal(..),
        (~>))
import Text.Ginger.Html (Html, htmlSource)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Readers.Creole as Pandoc

import Web.Templar.Backends
import Web.Templar.Cache
import Web.Templar.Logger as Logger
import Web.Templar.Project
import Web.Templar.ProjectConfig
import Web.Templar.Rule
import Web.Templar.ServerConfig
import Web.Templar.Backends.Loader.Type
       (PostBodySource (..), pbsFromRequest, pbsInvalid)
import Web.Templar.Handlers
       ( handleStaticTarget
       , handleNotFound
       , handleRedirectTarget
       , handleJSONTarget
       , handleTemplateTarget
       )
import Web.Templar.Handlers.Respond
import Web.Templar.Handlers.Common (loadBackendDict, handle500, handle404)

serveProject :: ServerConfig -> Project -> IO ()
serveProject config project = do
    forkIO vacuum
    serve project
    where
        serve = case scDriver config of
            DefaultDriver -> serveWarp Nothing
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

serveWarp :: Maybe Int -> Project -> IO ()
serveWarp portMay project = do
    writeLog (projectLogger project) Notice $
        "Finding port for Warp: " <> tshow portMay
    port <- case portMay of
        Just p -> return p
        Nothing -> do
            portEnvStr <- lookupEnv "PORT"
            let portEnv = fromMaybe 5000 $ portEnvStr >>= readMay
            return portEnv
    writeLog (projectLogger project) Notice $
        "Running server on port " <> tshow port <> "..."
    Warp.run port (appFromProject project)

serveCGI :: Project -> IO ()
serveCGI project = CGI.run (appFromProject project)

serveSCGI :: Project -> IO ()
serveSCGI project = SCGI.run (appFromProject project)

serveFastCGI :: Project -> IO ()
serveFastCGI project = FastCGI.run (appFromProject project)

appFromProject :: Project -> Wai.Application
appFromProject project request respond = do
    handleRequest project request respond `catch` handleException
    where
        handleException (e :: SomeException) = do
            writeLog (projectLogger project) Logger.Error $ tshow e
            respond $ Wai.responseLBS status500 [] "Something went pear-shaped."

handleRequest :: Project -> Wai.Application
handleRequest project request respond = do
    go `catchIOError` \e -> handle500 e project request respond
    where
        cache = projectBackendCache project
        go = do
            let path = Wai.pathInfo request
                query = queryToQueryText . Wai.queryString $ request
            let globalBackendPaths = pcContextData . projectConfig $ project
            case applyRules (pcRules . projectConfig $ project) path query of
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
