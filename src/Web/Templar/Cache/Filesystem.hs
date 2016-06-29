{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Web.Templar.Cache.Filesystem
where

import ClassyPrelude
import Data.Char (isAlphaNum, ord)
import Web.Templar.Cache
import System.IO (IOMode (..), withFile)
import System.Directory (removeFile)
import System.PosixCompat.Files (getFileStatus, modificationTime)
import Data.Time.Clock.POSIX

filesystemCache :: (k -> String) -- ^ Key serializer
                -> (Handle -> v -> IO ()) -- ^ Value serializer
                -> (Handle -> IO v) -- ^ Value deserializer
                -> FilePath -- ^ Base directory
                -> Cache k v -- ^ Resulting cache
filesystemCache serializeKey writeValue readValue cacheDir =
    Cache
        { cacheGet = \key -> do
            let filename = keyToFilename key
            catchIOError
                (do
                    status <- getFileStatus filename
                    let ts = realToFrac $ modificationTime status
                    body <- withFile filename ReadMode readValue
                    return $ Just (body, ts :: POSIXTime)
                )
                (\err -> if isDoesNotExistError err
                    then return Nothing
                    else ioError err)
        , cachePut = \key val -> do
            let filename = keyToFilename key
            withFile filename WriteMode (\h -> writeValue h val)
        , cacheDelete = \key -> do
            let filename = keyToFilename key
            catchIOError
                (removeFile filename)
                (\err -> if isDoesNotExistError err
                    then return ()
                    else ioError err
                )
        }
    where
        keyToFilename key = cacheDir </> encodeFilename (serializeKey key) <> ".cache"

encodeFilename :: String -> FilePath
encodeFilename =
    concatMap encodeChar
    where
        encodeChar :: Char -> FilePath
        encodeChar c
            | (c >= 'a' && c <= 'z') ||
              (c >= 'A' && c <= 'Z') ||
              (c >= '0' && c <= '9') = [c]
            | otherwise = '_' : show (ord c)
