{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Sprinkles.Handlers.Common
where

import ClassyPrelude
import Web.Sprinkles.Exceptions
import Web.Sprinkles.Backends
import qualified Network.Wai as Wai
import Web.Sprinkles.Logger as Logger
import Web.Sprinkles.Project
import Web.Sprinkles.ProjectConfig
import Network.HTTP.Types
       (Status, status200, status302, status400, status404, status405, status500)
import Web.Sprinkles.Handlers.Respond
import Text.Ginger.Html (Html, htmlSource, unsafeRawHtml)
import qualified Text.Ginger as Ginger
import Web.Sprinkles.Backends.Loader.Type
       (RequestContext (..), pbsFromRequest, pbsInvalid)
import Web.Sprinkles.Rule (expandReplacementBackend)
import Data.AList (AList)
import qualified Data.AList as AList
import Text.Ginger (GVal, ToGVal (..), Run, marshalGValEx, hoistRun)
import Text.Ginger.Run.VM (withEncoder)
import Control.Monad.Writer (Writer)
import Web.Sprinkles.SessionHandle

data NotFoundException = NotFoundException
    deriving (Show)

instance Exception NotFoundException where

data MethodNotAllowedException = MethodNotAllowedException
    deriving (Show)

instance Exception MethodNotAllowedException where

data NotAllowedException = NotAllowedException
    deriving (Show)

instance Exception NotAllowedException where

type ContextualHandler =
    HashMap Text (Items (BackendData IO Html)) ->
    Project ->
    Maybe SessionHandle ->
    Wai.Application

handleNotFound :: Project -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> NotFoundException -> IO Wai.ResponseReceived
handleNotFound project request respond _ = do
    handle404
        project
        request
        respond

handleMethodNotAllowed :: Project -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> MethodNotAllowedException -> IO Wai.ResponseReceived
handleMethodNotAllowed project request respond _ = do
    handle405
        project
        request
        respond

handleNotAllowed :: Project -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> NotAllowedException -> IO Wai.ResponseReceived
handleNotAllowed project request respond _ = do
    respond $ Wai.responseLBS
        status400
        [("Content-type", "text/plain")]
        "Not allowed"

handleHttpError :: Status
                -> Text
                -> Text
                -> Project
                -> Wai.Application
handleHttpError status templateName message project request respond =
    respondNormally `catch` handleTemplateNotFound
    where
        cache = projectBackendCache project
        backendPaths = pcContextData . projectConfig $ project
        logger = projectLogger project
        respondNormally = do
            backendData <- loadBackendDict
                                (writeLog logger)
                                (pbsFromRequest request Nothing)
                                cache
                                backendPaths
                                (setFromList [])
                                (mapFromList [])
            respondTemplateHtml
                project
                Nothing
                status
                templateName
                backendData
                request
                respond
        handleTemplateNotFound (e :: TemplateNotFoundException) = do
            writeLog logger Logger.Warning $ "Template " ++ templateName ++ " not found, using built-in fallback"
            let headers = [("Content-type", "text/plain;charset=utf8")]
            respond . Wai.responseLBS status headers . fromStrict . encodeUtf8 $ message

handle404 :: Project
          -> Wai.Application
handle404 = handleHttpError status404 "404.html" "Not Found"

handle405 :: Project
          -> Wai.Application
handle405 = handleHttpError status405 "405.html" "Method Not Allowed"

handle500 :: SomeException
          -> Project
          -> Wai.Application
handle500 err project request respond = do
    writeLog (projectLogger project) Logger.Error $ formatException err
    handleHttpError status500 "500.html" message project request respond
    where
        message = "Something went pear-shaped. The problem seems to be on our side."

loadBackendDict :: (LogLevel -> Text -> IO ())
                -> RequestContext
                -> RawBackendCache
                -> AList Text BackendSpec
                -> Set Text
                -> HashMap Text (GVal (Run IO Text))
                -> IO (HashMap Text (Items (BackendData IO Html)))
loadBackendDict writeLog postBodySrc cache backendPaths required globalContext = do
    mapFromList <$> go globalContext (AList.toList backendPaths)
    where
        go :: HashMap Text (GVal (Run IO Text))
           -> [(Text, BackendSpec)]
           -> IO [(Text, Items (BackendData IO Html))]
        go context ((key, backendSpec):specs) = do
            expBackendSpec <- expandReplacementBackend context backendSpec
            bd :: Items (BackendData IO Html)
               <- loadBackendData
                    writeLog
                    postBodySrc
                    cache
                    expBackendSpec
            resultItem <- case bd of
                NotFound ->
                    if key `elem` required
                        then throwM NotFoundException
                        else return (key, NotFound)
                _ -> return (key, bd)
            let bdG :: GVal (Run IO Html)
                bdG = toGVal bd
                bdGP :: GVal (Run IO Text)
                bdGP = marshalGValHtmlToText bdG
                context' = insertMap key bdGP context
            remainder <- go context' specs
            return $ resultItem:remainder
        go _ _ = return []

marshalGValHtmlToText :: GVal (Run IO Html) -> GVal (Run IO Text)
marshalGValHtmlToText = marshalGValEx hoistRunToText hoistRunFromText

hoistRunToText :: Run IO Html a -> Run IO Text a
hoistRunToText =
    hoistRun htmlSource unsafeRawHtml
        . withEncoder (unsafeRawHtml . Ginger.asText)
hoistRunFromText:: Run IO Text a -> Run IO Html a
hoistRunFromText =
    hoistRun unsafeRawHtml htmlSource

