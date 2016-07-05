{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | HTTP backend loader
module Web.Templar.Backends.Loader.HttpLoader
( httpLoader
, curlLoader
)
where

import ClassyPrelude
import qualified Network.HTTP as HTTP
import Web.Templar.Backends.Spec
        ( Credentials
        )
import Web.Templar.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        )
import Web.Templar.Logger (LogLevel (..))
import Web.Templar.Backends.Loader.Type
import Network.URI (parseURI, URI)
import System.FilePath (takeFileName, takeBaseName)
import Network.Curl (CurlOption (..))
import qualified Network.Curl as Curl
import Data.Char (isSpace)

httpLoader :: Text -> Credentials -> Loader
httpLoader uriText credentials writeLog fetchMode fetchOrder = do
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

curlLoader :: Text -> Credentials -> Loader
curlLoader uriText credentials writeLog fetchMode fetchOrder = do
    Curl.initialize >>= \curl -> do
        response <- Curl.curlGetResponse_
            (unpack uriText)
            [Curl.CurlFollowLocation True]
        let body = Curl.respBody response
            headersL = Curl.respHeaders response
            headers :: HashMap Text Text
            headers = mapFromList
                [(pack . toLower $ k, dropWhile isSpace . pack $ v) | (k, v) <- headersL ]
            getHeader :: Text -> Maybe Text
            getHeader hname = lookup hname headers
            getHeaderDef def = fromMaybe def . getHeader
            mimeType = encodeUtf8 $ getHeaderDef "text/plain" "content-type"
            contentLength = readMay . unpack =<< getHeader "content-length"
            meta = BackendMeta
                    { bmMimeType = mimeType
                    , bmMTime = Nothing
                    , bmName = pack . takeBaseName . unpack $ uriText
                    , bmPath = uriText
                    , bmSize = contentLength
                    }
        return [BackendSource meta body]

-- | Get a HTTP header value by header name from a list of headers.
lookupHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe String
lookupHeader name headers =
    headMay [ v | HTTP.Header n v <- headers, n == name ]
