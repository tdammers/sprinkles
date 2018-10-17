{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TypeApplications #-}

-- | File backend loader
module Web.Sprinkles.Backends.Loader.FileLoader
( fileLoader
)
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Verification (..)
        , Items (..)
        , reduceItems
        , RawBytes (..)
        , rawFromLBS, rawToLBS
        )
import System.FilePath (takeFileName, takeBaseName)
import System.FilePath.Glob (glob)
import System.Directory (doesDirectoryExist, getDirectoryContents)
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
import System.PosixCompat.Files
import System.IO (withBinaryFile, IOMode (..), hSeek, SeekMode (..))
import Data.ByteString (hGet)

fileLoader :: Text -> Loader
fileLoader filepath writeLog _ fetchMode fetchOrder =
    fetch `catchIOError` handle
    where
        filename = unpack filepath
        fetch = do
            writeLog Debug $ "FILE: " <> filepath
            candidates <- if '*' `elem` filename
                then glob filename
                else return [filename]
            mapM fetchOne candidates
        handle err
            | isDoesNotExistError err = do
                writeLog Notice $ "FILE: Not found: " <> filepath
                return []
            | otherwise = ioError err

        fetchOne candidate = do
            isDir <- doesDirectoryExist candidate
            if isDir
                then fetchAsDir candidate
                else fetchAsFile candidate
        fetchAsDir candidate = do
            let mimeType = "application/x-directory"
            contents <- (encodeUtf8 @LText . unlines . fmap pack <$> getDirectoryContents candidate)
                `catchIOError` \err -> do
                    writeLog Notice $ tshow err
                    return ""
            mkFileBackendSource mimeType candidate (rawFromLBS contents)

        fetchAsFile candidate = do
            let mimeType = mimeLookup . pack $ candidate
                contents = rawFromFile writeLog candidate
            mkFileBackendSource mimeType candidate contents

silenceIO :: (LogLevel -> Text -> IO ()) -> a -> IO a -> IO a
silenceIO writeLog defVal action =
    action
        `catchIOError` \err -> do
            writeLog Notice $ tshow err
            return defVal

rawFromFile :: (LogLevel -> Text -> IO ()) -> FilePath -> RawBytes
rawFromFile writeLog candidate =
    let getSize = silenceIO writeLog 0 $ fromIntegral . fileSize <$> getFileStatus candidate
        getRange start end = silenceIO writeLog "" $ do
            withBinaryFile candidate ReadMode $ \h -> do
                hSeek h AbsoluteSeek start
                fromStrict <$> hGet h (fromIntegral end)
    in RawBytes getSize getRange

mkFileBackendSource :: MimeType -> FilePath -> RawBytes -> IO BackendSource
mkFileBackendSource mimeType candidate contents = do
    status <- getFileStatus candidate
    let mtimeUnix = modificationTime status
        meta = BackendMeta
                { bmMimeType = mimeType
                , bmMTime = Just . realToFrac $ mtimeUnix
                , bmName = pack $ takeBaseName candidate
                , bmPath = pack candidate
                , bmSize = Just . fromIntegral $ fileSize status :: Maybe Integer
                }
    return $ BackendSource meta contents Trusted

-- | Our own extended MIME type dictionary. The default one lacks appropriate
-- entries for some of the important types we use, so we add them here.
mimeMap :: MimeMap
mimeMap =
    mapFromList
        [ ("yml", "application/x-yaml")
        , ("yaml", "application/x-yaml")
        , ("md", "application/x-markdown")
        , ("rst", "text/x-rst")
        , ("textile", "text/x-textile")
        , ("creole", "text/x-creole")
        , ("markdown", "application/x-markdown")
        , ("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
        ]
    <> defaultMimeMap

-- | Custom MIME lookup function that knows about the extra types declared in
-- 'mimeMap'.
mimeLookup :: FileName -> MimeType
mimeLookup = mimeByExt mimeMap defaultMimeType

