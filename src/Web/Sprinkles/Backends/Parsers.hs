{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}

-- | Parse raw backend data into useful data structures.
module Web.Sprinkles.Backends.Parsers
( parseBackendData
)
where

import ClassyPrelude

import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , toBackendData
        , addBackendDataChildren
        , rawToLBS, rawFromLBS
        )
import Web.Sprinkles.Backends.Spec
        ( parserTypes
        , ParserType (..)
        )
import Text.Pandoc (Pandoc)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.MediaBag as Pandoc
import qualified Text.Pandoc.Readers.Creole as Pandoc
import Text.Pandoc.Error (PandocError)
import Network.Mime (MimeType)
import Text.Ginger (ToGVal (..), GVal, Run (..), dict, (~>))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.Yaml as YAML
import Data.Char (ord)
import Web.Sprinkles.PandocGVal
import Network.HTTP.Types (parseQuery, queryToQueryText)

-- | Parse raw backend data source into a structured backend data record.
parseBackendData :: (MonadIO m, Monad m, Monad n)
                 => BackendSource
                 -> m (BackendData n h)
parseBackendData item@(BackendSource meta body) = do
    let t = takeWhile (/= fromIntegral (ord ';')) (bmMimeType meta)
        parse = fromMaybe parseRawData $ lookup t parsersTable
    parse item

-- | Lookup table of mime types to parsers.
parsersTable :: (MonadIO m, Monad m, Monad n)
             => HashMap MimeType (BackendSource -> m (BackendData n h))
parsersTable = mapFromList . mconcat $
    [ zip mimeTypes (repeat parser) | (mimeTypes, parser) <- parsers ]

-- | The parsers we know, by mime types.
parsers :: (MonadIO m, Monad m, Monad n)
        => [([MimeType], BackendSource -> m (BackendData n h))]
parsers =
    [ (types, getParser p) | (types, p) <- parserTypes ]

getParser :: (MonadIO m, Monad m, Monad n)
          => ParserType
          -> (BackendSource -> m (BackendData n h))
getParser ParserJSON = json
getParser ParserYAML = yaml
getParser ParserFormUrlencoded = urlencodedForm
getParser ParserMarkdown = pandoc (Pandoc.readMarkdown Pandoc.def)
getParser ParserCreole = pandoc (Pandoc.readCreole Pandoc.def)
getParser ParserTextile = pandoc (Pandoc.readTextile Pandoc.def)
getParser ParserRST = pandoc (Pandoc.readRST Pandoc.def)
getParser ParserLaTeX = pandoc (Pandoc.readLaTeX Pandoc.def)
getParser ParserDocX = pandocWithMedia (Pandoc.readDocx Pandoc.def)
getParser ParserText = plainText
getParser ParserHtml = pandoc (Pandoc.readHtml Pandoc.def)

-- | All the content types we know how to parse
knownContentTypes :: [MimeType]
knownContentTypes = concatMap fst parserTypes

-- | Parser for raw data (used for static files); this is also the default
-- fallback for otherwise unsupported file types.
parseRawData :: Monad m => BackendSource -> m (BackendData n h)
parseRawData (BackendSource meta body) =
    return BackendData
        { bdJSON = JSON.Null
        , bdGVal = toGVal JSON.Null
        , bdMeta = meta
        , bdRaw = body
        , bdChildren = mapFromList []
        }

-- | Parser for (utf-8) plaintext documents.
plainText :: (MonadIO m, Monad m) => BackendSource -> m (BackendData n h)
plainText item@(BackendSource meta body) = do
    textBody <- liftIO $ toStrict . decodeUtf8 <$> rawToLBS body
    return $ toBackendData item textBody

-- | Parser for JSON source data.
json :: (MonadIO m, Monad m) => BackendSource -> m (BackendData n h)
json item@(BackendSource meta body) = do
    bodyBytes <- liftIO $ rawToLBS body
    case JSON.eitherDecode bodyBytes of
        Left err -> fail $ err ++ "\n" ++ show bodyBytes
        Right json -> return . toBackendData item $ (json :: JSON.Value)

-- | Parser for YAML source data.
yaml :: (MonadIO m, Monad m) => BackendSource -> m (BackendData n h)
yaml item@(BackendSource meta body) = do
    bodyBytes <- liftIO $ rawToLBS body
    case YAML.decodeEither (toStrict bodyBytes) of
        Left err -> fail $ err ++ "\n" ++ show bodyBytes
        Right json -> return . toBackendData item $ (json :: JSON.Value)

urlencodedForm :: (MonadIO m, Monad m) => BackendSource -> m (BackendData n h)
urlencodedForm item@(BackendSource meta body) = do
    bodyBytes <- liftIO $ rawToLBS body
    return .
        toBackendData item .
        asTextHashMap .
        mapFromList .
        queryToQueryText .
        parseQuery .
        toStrict $ bodyBytes
    where
        asTextHashMap :: HashMap Text (Maybe Text) -> HashMap Text (Maybe Text)
        asTextHashMap = id

-- | Parser for Pandoc-supported formats that are read from 'LByteString's.
pandocBS :: (MonadIO m, Monad m, Monad n)
         => (LByteString -> Either PandocError Pandoc)
         -> BackendSource
         -> m (BackendData n h)
pandocBS reader input@(BackendSource meta body) = do
    bodyBytes <- liftIO $ rawToLBS body
    case reader bodyBytes of
        Left err -> fail . show $ err
        Right pandoc -> return $ toBackendData input pandoc

-- | Parser for Pandoc-supported formats that are read from 'LByteString's, and
-- return a 'Pandoc' document plus a 'MediaBag'.
pandocWithMedia :: (MonadIO m, Monad m, Monad n)
                => (LByteString -> Either PandocError (Pandoc, Pandoc.MediaBag))
                -> BackendSource
                -> m (BackendData n h)
pandocWithMedia reader input@(BackendSource meta body) = do
    bodyBytes <- liftIO $ rawToLBS body
    case reader bodyBytes of
        Left err -> fail . show $ err
        Right (pandoc, mediaBag) -> do
            -- TODO: marshal mediaBag to backend data item children
            let base = toBackendData input pandoc
            children <- mapFromList <$> mediaBagToBackendData mediaBag
            return $ addBackendDataChildren children base

mediaBagToBackendData :: (MonadIO m, Monad m, Monad n)
                      => Pandoc.MediaBag
                      -> m [(Text, BackendData n h)]
mediaBagToBackendData bag = do
    let metas = Pandoc.mediaDirectory bag
    forM metas $ \(path, mimeType, contentLength) -> do
        (_, body) <- maybe
                        (fail $ "Media not found: " <> path)
                        return
                        (Pandoc.lookupMedia path bag)
        let meta =
                BackendMeta
                    { bmMimeType = encodeUtf8 . pack $ mimeType
                    , bmMTime = Nothing
                    , bmName = pack path
                    , bmPath = pack path
                    , bmSize = Just $ fromIntegral contentLength
                    }
        (pack path,) <$> parseBackendData (BackendSource meta (rawFromLBS body))

-- | Parser for Pandoc-supported formats that are read from 'String's.
pandoc :: (MonadIO m, Monad m, Monad n)
       => (String -> Either PandocError Pandoc)
       -> BackendSource
       -> m (BackendData n h)
pandoc reader =
    pandocBS (reader . unpack . decodeUtf8)

