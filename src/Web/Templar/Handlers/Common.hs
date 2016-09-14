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
module Web.Templar.Handlers.Common
where

import ClassyPrelude
import Web.Templar.Backends
import qualified Network.Wai as Wai
import Web.Templar.Logger as Logger
import Web.Templar.Project
import Web.Templar.ProjectConfig
import Network.HTTP.Types
       (Status, status200, status302, status400, status404, status500)
import Web.Templar.Handlers.Respond
import Text.Ginger.Html (Html, htmlSource)
import Web.Templar.Backends.Loader.Type
       (PostBodySource (..), pbsFromRequest, pbsInvalid)

data NotFoundException = NotFoundException
    deriving (Show)

instance Exception NotFoundException where

handleNotFound :: Project -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> NotFoundException -> IO Wai.ResponseReceived
handleNotFound project request respond _ = do
    let globalBackendPaths = pcContextData . projectConfig $ project
    handle404
        (globalBackendPaths, setFromList [])
        project
        request
        respond

handle404 :: (HashMap Text BackendSpec, Set Text)
          -> Project
          -> Wai.Application
handle404 (backendPaths, required) project request respond = do
    respondNormally `catch` handleTemplateNotFound
    where
        cache = projectBackendCache project
        logger = projectLogger project
        respondNormally = do
            backendData <- loadBackendDict (writeLog logger) (pbsFromRequest request) cache backendPaths required
            respondTemplateHtml
                project
                status404
                "404.html"
                backendData
                request
                respond
        handleTemplateNotFound (e :: TemplateNotFoundException) = do
            writeLog logger Logger.Warning $ "Template 404.html not found, using built-in fallback"
            let headers = [("Content-type", "text/plain;charset=utf8")]
            respond . Wai.responseLBS status404 headers $ "Not Found"

handle500 :: Show e
          => e
          -> Project
          -> Wai.Application
handle500 err project request respond = do
    writeLog logger Logger.Error $ tshow err
    respondNormally `catch` handleTemplateNotFound
    where
        cache = projectBackendCache project
        backendPaths = pcContextData . projectConfig $ project
        logger = projectLogger project
        respondNormally = do
            backendData <- loadBackendDict (writeLog logger) (pbsFromRequest request) cache backendPaths (setFromList [])
            respondTemplateHtml
                project
                status500
                "500.html"
                backendData
                request
                respond
        handleTemplateNotFound (e :: TemplateNotFoundException) = do
            writeLog logger Logger.Warning $ "Template 500.html not found, using built-in fallback"
            let headers = [("Content-type", "text/plain;charset=utf8")]
            respond . Wai.responseLBS status500 headers $ "Something went pear-shaped. The problem seems to be on our side."

loadBackendDict :: (LogLevel -> Text -> IO ()) -> PostBodySource -> RawBackendCache -> HashMap Text BackendSpec -> Set Text -> IO (HashMap Text (Items (BackendData IO Html)))
loadBackendDict writeLog postBodySrc cache backendPaths required = do
    pairs <- forM (mapToList backendPaths) $ \(key, backendPath) -> do
        bd :: Items (BackendData IO Html) <- loadBackendData writeLog postBodySrc cache backendPath
        case bd of
            NotFound ->
                if key `elem` required
                    then throwM NotFoundException
                    else return $ (key, NotFound)
            _ -> return (key, bd)
    return $ mapFromList pairs
