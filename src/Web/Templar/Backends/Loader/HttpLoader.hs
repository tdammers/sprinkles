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

-- | Get a HTTP header value by header name from a list of headers.
lookupHeader :: HTTP.HeaderName -> [HTTP.Header] -> Maybe String
lookupHeader name headers =
    headMay [ v | HTTP.Header n v <- headers, n == name ]
