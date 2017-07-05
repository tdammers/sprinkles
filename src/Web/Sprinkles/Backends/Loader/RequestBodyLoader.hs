{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE DeriveGeneric #-}

-- | File backend loader
module Web.Sprinkles.Backends.Loader.RequestBodyLoader
( requestBodyLoader
)
where

import ClassyPrelude
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
import Network.Mime
            ( MimeType
            , MimeMap
            , defaultMimeLookup
            , defaultMimeMap
            , mimeByExt
            , defaultMimeType
            , FileName
            )
import qualified Data.ByteString as BS
import Data.Char (ord)

data Disposition =
    Disposition
        { baseDisposition :: ByteString
        , dispositionAttribs :: [(ByteString, ByteString)]
        }
    deriving (Show, Eq, Generic)

trimL :: ByteString -> ByteString
trimL = BS.dropWhile (<= 32)

trimR :: ByteString -> ByteString
trimR = fst . BS.spanEnd (<= 32)

trim :: ByteString -> ByteString
trim = trimL . trimR

stripQuotes :: ByteString -> ByteString
stripQuotes =
    BS.takeWhile (/= fromIntegral (ord '"')) .
    BS.dropWhile (== fromIntegral (ord '"'))

parseDisposition :: ByteString -> Disposition
parseDisposition str =
    let base:attribStrs = map trim $ BS.split (fromIntegral . ord $ ';') str
        attribs = concatMap parseAttrib attribStrs
    in Disposition base attribs
    where
        parseAttrib :: ByteString -> [(ByteString, ByteString)]
        parseAttrib str =
            case BS.split (fromIntegral . ord $ '=') str of
                [name, value] ->
                    [(trim name, stripQuotes . trim $ value)]
                _ -> []

requestBodyLoader :: Loader
requestBodyLoader writeLog pbs fetchMode fetchOrder = do
    contents <- rawFromLBS <$> loadPost pbs
    let disposition =
            fromMaybe (Disposition "attachment" []) $
            parseDisposition <$> lookupHeader pbs "Content-Disposition"
        filename = fromMaybe "POST" $
            lookup "filename" (dispositionAttribs disposition)
    writeLog Debug . pack . show $ disposition
    let meta = BackendMeta
                { bmMimeType = contentType pbs
                , bmMTime = Nothing
                , bmName = "POST"
                , bmPath = decodeUtf8 filename
                , bmSize = Nothing
                }
    return [BackendSource meta contents VerifyCSRF]
