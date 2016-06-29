{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | File backend loader
module Web.Templar.Backends.Loader.FileLoader
( fileLoader
)
where

import ClassyPrelude
import Web.Templar.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        )
import System.FilePath (takeFileName, takeBaseName)
import System.FilePath.Glob (glob)
import Web.Templar.Logger (LogLevel (..))
import Web.Templar.Backends.Loader.Type
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

fileLoader :: Text -> Loader
fileLoader filepath writeLog fetchMode fetchOrder =
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
            let mimeType = mimeLookup . pack $ candidate
            contents <- readFile candidate `catchIOError` \err -> do
                writeLog Notice $ tshow err
                return ""
            status <- getFileStatus candidate
            let mtimeUnix = modificationTime status
                meta = BackendMeta
                        { bmMimeType = mimeType
                        , bmMTime = Just mtimeUnix
                        , bmName = pack $ takeBaseName candidate
                        , bmPath = pack candidate
                        , bmSize = (Just . fromIntegral $ fileSize status :: Maybe Integer)
                        }
            return $ BackendSource meta contents

-- | Our own extended MIME type dictionary. The default one lacks appropriate
-- entries for some of the important types we use, so we add them here.
mimeMap :: MimeMap
mimeMap =
    defaultMimeMap <>
    mapFromList
        [ ("yml", "application/x-yaml")
        , ("yaml", "application/x-yaml")
        , ("md", "application/x-markdown")
        , ("rst", "text/x-rst")
        , ("textile", "text/x-textile")
        , ("markdown", "application/x-markdown")
        , ("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document")
        ]

-- | Custom MIME lookup function that knows about the extra types declared in
-- 'mimeMap'.
mimeLookup :: FileName -> MimeType
mimeLookup = mimeByExt mimeMap defaultMimeType
