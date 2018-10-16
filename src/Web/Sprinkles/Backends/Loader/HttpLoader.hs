{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | HTTP backend loader
module Web.Sprinkles.Backends.Loader.HttpLoader
( curlLoader
)
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Backends.Spec
        ( HttpBackendOptions (..)
        )
import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Verification (..)
        , Items (..)
        , reduceItems
        , rawFromLBS
        )
import Web.Sprinkles.Logger (LogLevel (..))
import Web.Sprinkles.Backends.Loader.Type
import Network.URI (parseURI, URI)
import System.FilePath (takeFileName, takeBaseName)
import Network.Curl (CurlOption (..))
import qualified Network.Curl as Curl
import Data.Char (isSpace)

data HttpError = HttpError String
    deriving (Show, Eq)

instance Exception HttpError where

curlLoader :: Text -> HttpBackendOptions -> Loader
curlLoader uriText options writeLog _ fetchMode fetchOrder = do
    let accepts = intercalate "," . map (unpack . decodeUtf8) $ httpAcceptedContentTypes options
    writeLog Debug $ "cURL " <> uriText
    Curl.initialize >>= \curl -> do
        response <- Curl.curlGetResponse_
            (unpack uriText)
            [ Curl.CurlFollowLocation True
            , Curl.CurlPostRedirect True
            , Curl.CurlFailOnError False
            , Curl.CurlUserAgent "sprinkles https://sprinkles.tobiasdammers.nl/"
            , Curl.CurlHttpHeaders
                [ "Accept: " ++ accepts ]
            ]
        let headersL = Curl.respHeaders response
            headers :: HashMap Text Text
            headers = mapFromList
                [(pack . toLower $ k, dropWhile isSpace . pack $ v) | (k, v) <- headersL ]
            getHeader :: Text -> Maybe Text
            getHeader hname = lookup hname headers
            getHeaderDef def = fromMaybe def . getHeader
            mimeType = encodeUtf8 $ getHeaderDef "text/plain" "content-type"
            contentLength = readMay . unpack =<< getHeader "content-length"
        writeLog Debug $ (pack . show) (Curl.respCurlCode response)
        writeLog Debug $ pack (Curl.respStatusLine response)
        writeLog Debug $ "Content-type: " <> decodeUtf8 mimeType
        writeLog Debug $ "Content-length: " <> maybe "?" (pack . show) contentLength
        if Curl.respStatus response /= 200
            then do
                writeLog Warning $ "HTTP error: " <> uriText <> " - " <> pack (Curl.respStatusLine response)
                return []
            else do
                -- TODO: support Range requests on the backend
                let body = rawFromLBS $ Curl.respBody response
                    meta = BackendMeta
                            { bmMimeType = mimeType
                            , bmMTime = Nothing
                            , bmName = pack . takeBaseName . unpack $ uriText
                            , bmPath = uriText
                            , bmSize = contentLength
                            }
                return [BackendSource meta body Trusted]
