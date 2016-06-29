{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Web.Templar.Cache.Filesystem
where

import ClassyPrelude
import Data.Char (isAlphaNum, ord, isDigit, isAlpha, chr)
import Prelude (read)
import Web.Templar.Cache
import System.IO (IOMode (..), withFile)
import System.Directory (removeFile, getDirectoryContents)
import System.FilePath (takeFileName)
import System.PosixCompat.Files (getFileStatus, modificationTime)
import Data.Time.Clock.POSIX

filesystemCache :: (k -> String) -- ^ Key serializer
                -> (String -> k) -- ^ Key deserializer
                -> (Handle -> v -> IO ()) -- ^ Value serializer
                -> (Handle -> IO v) -- ^ Value deserializer
                -> FilePath -- ^ Base directory
                -> Cache k v -- ^ Resulting cache
filesystemCache serializeKey deserializeKey writeValue readValue cacheDir =
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
        , cacheKeys = do
            filenames <- filter (".cache" `isSuffixOf`) <$> getDirectoryContents cacheDir
            forM filenames $ \filename -> do
                let key = keyFromFilename filename
                status <- getFileStatus (cacheDir </> filename)
                let ts = realToFrac $ modificationTime status
                return (key, ts)
        }
    where
        keyToFilename key = cacheDir </> encodeFilename (serializeKey key) <> ".cache"
        keyFromFilename = deserializeKey . decodeFilename . takeFileName

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

decodeFilename :: FilePath -> String
decodeFilename "" = ""
decodeFilename ('_':xs) =
    let (intpart, remainder) = span isDigit xs
    in chr (read intpart):decodeFilename remainder
decodeFilename ('.':xs) = ""
decodeFilename (x:xs)
    | isAlpha x = x:decodeFilename xs
    | otherwise = decodeFilename xs
