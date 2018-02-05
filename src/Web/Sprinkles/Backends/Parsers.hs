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
        , Verification (..)
        , toBackendData
        , addBackendDataChildren
        , rawToLBS, rawFromLBS
        )
import Web.Sprinkles.Backends.Spec
        ( parserTypes
        , ParserType (..)
        )
import Web.Sprinkles.Pandoc (pandocReaderOptions)
import Text.Pandoc (Pandoc, PandocPure)
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
                 -> m (BackendData p n h)
parseBackendData item@(BackendSource meta body _) = do
    let t = takeWhile (/= fromIntegral (ord ';')) (bmMimeType meta)
        parse = fromMaybe parseRawData $ lookup t parsersTable
    parse item

-- | Lookup table of mime types to parsers.
parsersTable :: (MonadIO m, Monad m, Monad n)
             => HashMap MimeType (BackendSource -> m (BackendData p n h))
parsersTable = mapFromList . mconcat $
    [ zip mimeTypes (repeat parser) | (mimeTypes, parser) <- parsers ]

-- | The parsers we know, by mime types.
parsers :: (MonadIO m, Monad m, Monad n)
        => [([MimeType], BackendSource -> m (BackendData p n h))]
parsers =
    [ (types, getParser p) | (types, p) <- parserTypes ]

getParser :: (MonadIO m, Monad m, Monad n)
          => ParserType
          -> (BackendSource -> m (BackendData p n h))
getParser ParserJSON = json
getParser ParserYAML = yaml
getParser ParserFormUrlencoded = urlencodedForm
getParser ParserMarkdown = pandoc (Pandoc.readMarkdown pandocReaderOptions)
getParser ParserCreole = pandoc (Pandoc.readCreole pandocReaderOptions)
getParser ParserTextile = pandoc (Pandoc.readTextile pandocReaderOptions)
getParser ParserRST = pandoc (Pandoc.readRST pandocReaderOptions)
getParser ParserLaTeX = pandoc (Pandoc.readLaTeX pandocReaderOptions)
getParser ParserDocX = pandocWithMedia (Pandoc.readDocx pandocReaderOptions)
getParser ParserText = plainText
getParser ParserHtml = pandoc (Pandoc.readHtml pandocReaderOptions)

-- | All the content types we know how to parse
knownContentTypes :: [MimeType]
knownContentTypes = concatMap fst parserTypes

-- | Parser for raw data (used for static files); this is also the default
-- fallback for otherwise unsupported file types.
parseRawData :: Monad m => BackendSource -> m (BackendData p n h)
parseRawData (BackendSource meta body veri) =
    return BackendData
        { bdJSON = JSON.Null
        , bdGVal = toGVal JSON.Null
        , bdMeta = meta
        , bdRaw = body
        , bdChildren = mapFromList []
        , bdVerification = veri
        }

-- | Parser for (utf-8) plaintext documents.
plainText :: (MonadIO m, Monad m) => BackendSource -> m (BackendData p n h)
plainText item@(BackendSource meta body _) = do
    textBody <- liftIO $ toStrict . decodeUtf8 <$> rawToLBS body
    return $ toBackendData item textBody

-- | Parser for JSON source data.
json :: (MonadIO m, Monad m) => BackendSource -> m (BackendData p n h)
json item@(BackendSource meta body _) = do
    bodyBytes <- liftIO $ rawToLBS body
    case JSON.eitherDecode bodyBytes of
        Left err -> fail $ err ++ "\n" ++ show bodyBytes
        Right json -> return . toBackendData item $ (json :: JSON.Value)

-- | Parser for YAML source data.
yaml :: (MonadIO m, Monad m) => BackendSource -> m (BackendData p n h)
yaml item@(BackendSource meta body _) = do
    bodyBytes <- liftIO $ rawToLBS body
    case YAML.decodeEither (toStrict bodyBytes) of
        Left err -> fail $ err ++ "\n" ++ show bodyBytes
        Right json -> return . toBackendData item $ (json :: JSON.Value)

urlencodedForm :: (MonadIO m, Monad m) => BackendSource -> m (BackendData p n h)
urlencodedForm item@(BackendSource meta body _) = do
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
         => (LByteString -> PandocPure Pandoc)
         -> BackendSource
         -> m (BackendData p n h)
pandocBS reader input@(BackendSource meta body _) = do
    bodyBytes <- liftIO $ rawToLBS body
    case Pandoc.runPure $ reader bodyBytes of
        Left err -> fail . show $ err
        Right pandoc -> return $ toBackendData input pandoc

-- | Parser for Pandoc-supported formats that are read from 'LByteString's, and
-- return a 'Pandoc' document plus a 'MediaBag'.
pandocWithMedia :: (MonadIO m, Monad m, Monad n)
                => (LByteString -> PandocPure Pandoc)
                -> BackendSource
                -> m (BackendData p n h)
pandocWithMedia reader input@(BackendSource meta body _) = do
    bodyBytes <- liftIO $ rawToLBS body
    let reader' = do
          pandoc <- reader bodyBytes
          mediaBag <- Pandoc.getMediaBag
          return (pandoc, mediaBag)
    case Pandoc.runPure reader' of
        Left err -> fail . show $ err
        Right (pandoc, mediaBag) -> do
            -- TODO: marshal mediaBag to backend data item children
            let base = toBackendData input pandoc
            children <- mapFromList <$> mediaBagToBackendData mediaBag
            return $ addBackendDataChildren children base

mediaBagToBackendData :: (MonadIO m, Monad m, Monad n)
                      => Pandoc.MediaBag
                      -> m [(Text, BackendData p n h)]
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
        (pack path,) <$> parseBackendData (BackendSource meta (rawFromLBS body) Trusted)

-- | Parser for Pandoc-supported formats that are read from 'String's.
pandoc :: (MonadIO m, Monad m, Monad n)
       => (Text -> PandocPure Pandoc)
       -> BackendSource
       -> m (BackendData p n h)
pandoc reader =
    pandocBS (reader . toStrict . decodeUtf8)

