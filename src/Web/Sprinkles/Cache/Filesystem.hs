{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Sprinkles.Cache.Filesystem
where

import ClassyPrelude
import Data.Char (isAlphaNum, ord, isDigit, isAlpha, chr)
import Prelude (read)
import Web.Sprinkles.Cache
import System.IO (IOMode (..), withFile)
import System.Directory (removeFile, getDirectoryContents)
import System.FilePath (takeFileName)
import System.PosixCompat.Files (getFileStatus, modificationTime)
import Data.Time.Clock.POSIX
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

ignoreNonexisting :: a -> IOError -> IO a
ignoreNonexisting r err =
    if isDoesNotExistError err
        then return r
        else ioError err

ignoreNonexisting_ :: IOError -> IO ()
ignoreNonexisting_ = ignoreNonexisting ()

filesystemCache :: (k -> String) -- ^ Key serializer
                -> (String -> k) -- ^ Key deserializer
                -> (Handle -> v -> IO ()) -- ^ Value serializer
                -> (Handle -> IO v) -- ^ Value deserializer
                -> FilePath -- ^ Base directory
                -> POSIXTime -- ^ Expiration, in seconds
                -> Cache k v -- ^ Resulting cache
filesystemCache serializeKey deserializeKey writeValue readValue cacheDir maxAge =
    Cache
        { cacheGet = \key -> do
            let filename = keyToFilename key
            catchIOError
                (do
                    status <- getFileStatus filename
                    body <- withFile filename ReadMode readValue
                    return $ Just body
                )
                (ignoreNonexisting Nothing)
        , cachePut = \key val -> do
            let filename = keyToFilename key
            withFile filename WriteMode (\h -> writeValue h val)
        , cacheDelete = \key -> do
            let filename = keyToFilename key
            removeFile filename `catchIOError` ignoreNonexisting_
        , cacheVacuum = do
            filenames <- map (cacheDir </>) . filter (".cache" `isSuffixOf`) <$> getDirectoryContents cacheDir
            timestamped <- forM filenames $ \filename -> do
                status <- getFileStatus filename
                let ts = realToFrac $ modificationTime status
                return (filename, ts)
            now <- getPOSIXTime
            let expirationTS = now - maxAge
                expired = map fst . filter (\(_, ts) -> ts < expirationTS) $ timestamped
            forM_ expired $ \filename ->
                removeFile filename
                `catchIOError` ignoreNonexisting_
            return $ length expired
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
            | isAsciiLower c ||
              isAsciiUpper c ||
              isDigit c = [c]
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
