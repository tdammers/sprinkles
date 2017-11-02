{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE DeriveGeneric #-}
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
import Web.Sprinkles.Backends.Data
       (BackendData (..), BackendSource (..), Verification (..))
import Web.Sprinkles.Rule (expandReplacementBackend)
import Data.AList (AList)
import qualified Data.AList as AList
import Text.Ginger (GVal, ToGVal (..), Run, marshalGValEx, hoistRun, SourcePos)
import Text.Ginger.Run.VM (withEncoder)
import Control.Monad.Writer (Writer)
import Web.Sprinkles.SessionHandle
import Web.Sprinkles.Exceptions
import qualified Data.Foldable as Foldable
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON (..))
import Text.Printf (printf)

data NotFoundException = NotFoundException
    deriving (Show, Eq, Generic)

instance Exception NotFoundException where

data MethodNotAllowedException = MethodNotAllowedException
    deriving (Show, Eq, Generic)

instance Exception MethodNotAllowedException where

data NotAllowedException = NotAllowedException
    deriving (Show, Eq, Generic)

instance Exception NotAllowedException where

data RequestValidationException = RequestValidationException
    deriving (Show, Eq, Generic)

instance Exception RequestValidationException

type ContextualHandler =
    HashMap Text (Items (BackendData SourcePos IO Html)) ->
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

handleRequestValidation :: Project -> Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> RequestValidationException -> IO Wai.ResponseReceived
handleRequestValidation project request respond _ = do
    respond $ Wai.responseLBS
        status400
        [("Content-type", "text/plain")]
        "Invalid Request"

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
                -> HashMap Text (GVal (Run SourcePos IO Text))
                -> IO (HashMap Text (Items (BackendData SourcePos IO Html)))
loadBackendDict writeLog reqCtx cache backendPaths required globalContext = do
    mapFromList <$> go globalContext (AList.toList backendPaths)
    where
        go :: HashMap Text (GVal (Run SourcePos IO Text))
           -> [(Text, BackendSpec)]
           -> IO [(Text, Items (BackendData SourcePos IO Html))]
        go context ((key, backendSpec):specs) = do
            expBackendSpec <- expandReplacementBackend context backendSpec
            bd :: Items (BackendData SourcePos IO Html)
               <- loadBackendData
                    writeLog
                    reqCtx
                    cache
                    expBackendSpec
            Foldable.traverse_ (verifyBD writeLog reqCtx) bd
            resultItem <- case bd of
                NotFound ->
                    if key `elem` required
                        then throwM NotFoundException
                        else return (key, NotFound)
                _ -> return (key, bd)
            let bdG :: GVal (Run SourcePos IO Html)
                bdG = toGVal bd
                bdGP :: GVal (Run SourcePos IO Text)
                bdGP = marshalGValHtmlToText bdG
                context' = insertMap key bdGP context
            remainder <- go context' specs
            return $ resultItem:remainder
        go _ _ = return []

verifyBD :: (LogLevel -> Text -> IO ()) -> RequestContext -> BackendData SourcePos IO Html -> IO ()
verifyBD writeLog reqCtx bd =
    case bdVerification bd of
        Trusted -> do
            writeLog Debug "Trusted"
            return ()
        VerifyCSRF -> do
            writeLog Debug "CSRF"
            let csrfHeaderMay = decodeUtf8 <$> lookupHeader reqCtx "X-Form-Token"
                csrfFormFieldMay =
                    (fromJSONMay (bdJSON bd) :: Maybe (HashMap Text JSON.Value))
                    >>= lookup "__form_token"
                    >>= fromJSONMay
            writeLog Debug $ "POST (JSON): " <> tshow (bdJSON bd)
            case sessionHandle reqCtx of
                Nothing -> do
                    -- No session means there's no need to check the
                    -- CSRF token, because without a session, the user
                    -- cannot be holding an authenticated identity.
                    writeLog Notice "No session, not performing CSRF validation"
                    return ()
                Just session -> do
                    writeLog Notice "Session found, checking CSRF token"
                    csrfToken <- maybe
                        (throwM RequestValidationException)
                        return
                        =<< sessionGet session "csrf"
                    let candidates :: [Text]
                        candidates = catMaybes [csrfHeaderMay, csrfFormFieldMay]
                    writeLog Notice . pack $ printf "CSRF token: %s; candidates: %s"
                        (show csrfToken) (show candidates)
                    when (null candidates)
                        (throwM RequestValidationException)
                    when (any (/= csrfToken) candidates)
                        (throwM RequestValidationException)

fromJSONMay :: FromJSON a => JSON.Value -> Maybe a
fromJSONMay x = case JSON.fromJSON x of
    JSON.Error _ -> Nothing
    JSON.Success a -> Just a

marshalGValHtmlToText :: GVal (Run SourcePos IO Html) -> GVal (Run SourcePos IO Text)
marshalGValHtmlToText = marshalGValEx hoistRunToText hoistRunFromText

hoistRunToText :: Run SourcePos IO Html a -> Run SourcePos IO Text a
hoistRunToText =
    hoistRun htmlSource unsafeRawHtml
        . withEncoder (unsafeRawHtml . Ginger.asText)
hoistRunFromText:: Run SourcePos IO Text a -> Run SourcePos IO Html a
hoistRunFromText =
    hoistRun unsafeRawHtml htmlSource

