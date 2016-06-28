{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Parse raw backend data into useful data structures.
module Web.Templar.Backends.Parsers
( parseBackendData
)
where

import ClassyPrelude

import Web.Templar.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , toBackendData
        )
import qualified Text.Pandoc as Pandoc
import Text.Pandoc (Pandoc)
import Text.Pandoc.Error (PandocError)
import Network.Mime (MimeType)
import Text.Ginger (ToGVal (..), GVal, Run (..), dict, (~>))
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import Data.Char (ord)
import Web.Templar.PandocGVal

-- | Parse raw backend data source into a structured backend data record.
parseBackendData :: Monad m => BackendSource -> m (BackendData n h)
parseBackendData item@(BackendSource meta body) = do
    let t = takeWhile (/= fromIntegral (ord ';')) (bmMimeType meta)
        parse = fromMaybe parseRawData $ lookup t parsersTable
    parse item

-- | Lookup table of mime types to parsers.
parsersTable :: Monad m => HashMap MimeType (BackendSource -> m (BackendData n h))
parsersTable = mapFromList . mconcat $
    [ zip mimeTypes (repeat parser) | (mimeTypes, parser) <- parsers ]

-- | The parsers we know, by mime types.
parsers :: Monad m => [([MimeType], (BackendSource -> m (BackendData n h)))]
parsers =
    [ ( ["application/json", "text/json"]
      , parseJSONData
      )
    , ( ["text/plain"]
      , parsePlainText
      )
    , ( ["application/x-yaml", "text/x-yaml", "application/yaml", "text/yaml"]
      , parseYamlData
      )
    , ( ["application/x-markdown", "text/x-markdown"]
      , parsePandocDataString (Pandoc.readMarkdown Pandoc.def)
      )
    , ( ["application/x-textile", "text/x-textile"]
      , parsePandocDataString (Pandoc.readTextile Pandoc.def)
      )
    , ( ["application/x-rst", "text/x-rst"]
      , parsePandocDataString (Pandoc.readRST Pandoc.def)
      )
    , ( ["application/html", "text/html"]
      , parsePandocDataString (Pandoc.readHtml Pandoc.def)
      )
    , ( ["application/vnd.openxmlformats-officedocument.wordprocessingml.document"]
      , parsePandocDataLBS (fmap fst . Pandoc.readDocx Pandoc.def)
      )
    ]

-- | Parser for raw data (used for static files); this is also the default
-- fallback for otherwise unsupported file types.
parseRawData :: Monad m => BackendSource -> m (BackendData n h)
parseRawData (BackendSource meta body) =
    return $ BackendData
        { bdJSON = JSON.Null
        , bdGVal = toGVal JSON.Null
        , bdMeta = meta
        , bdRaw = body
        }

-- | Parser for (utf-8) plaintext documents.
parsePlainText :: Monad m => BackendSource -> m (BackendData n h)
parsePlainText item@(BackendSource meta body) = do
    let textBody = toStrict $ decodeUtf8 body
    return $ toBackendData item textBody

-- | Parser for JSON source data.
parseJSONData :: Monad m => BackendSource -> m (BackendData n h)
parseJSONData item@(BackendSource meta body) =
    case JSON.eitherDecode body of
        Left err -> fail $ err ++ "\n" ++ show body
        Right json -> return . toBackendData item $ (json :: JSON.Value)

-- | Parser for YAML source data.
parseYamlData :: Monad m => BackendSource -> m (BackendData n h)
parseYamlData item@(BackendSource meta body) =
    case YAML.decodeEither (toStrict body) of
        Left err -> fail $ err ++ "\n" ++ show body
        Right json -> return . toBackendData item $ (json :: JSON.Value)

-- | Parser for Pandoc-supported formats that are read from 'LByteString's.
parsePandocDataLBS :: Monad m
                   => (LByteString -> Either PandocError Pandoc)
                   -> BackendSource
                   -> m (BackendData n h)
parsePandocDataLBS reader input@(BackendSource meta body) = do
    case reader body of
        Left err -> fail . show $ err
        Right pandoc -> return $ toBackendData input pandoc

-- | Parser for Pandoc-supported formats that are read from 'String's.
parsePandocDataString :: Monad m
                   => (String -> Either PandocError Pandoc)
                   -> BackendSource
                   -> m (BackendData n h)
parsePandocDataString reader =
    parsePandocDataLBS (reader . unpack . decodeUtf8)

