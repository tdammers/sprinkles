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
( curlLoader
)
where

import ClassyPrelude
import Web.Templar.Backends.Spec
        ( HttpBackendOptions (..)
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

curlLoader :: Text -> HttpBackendOptions -> Loader
curlLoader uriText options writeLog fetchMode fetchOrder = do
    Curl.initialize >>= \curl -> do
        response <- Curl.curlGetResponse_
            (unpack uriText)
            [ Curl.CurlFollowLocation True
            , Curl.CurlPostRedirect True
            ]
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
