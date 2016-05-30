{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}

-- | CLI program that drives a Templar instance.
module Main where

import ClassyPrelude
import Web.Templar
import Text.Read (read, readMaybe)
import Data.Default (def)

parseArgs :: [Text] -> IO ServerConfig
parseArgs = parseArgs' . reverse

parseArgs' :: [Text] -> IO ServerConfig
parseArgs' [] = return def
parseArgs' ("-warp":xs) = do
    let portMay = readMaybe . unpack =<< headMay xs
    case portMay of
        Nothing -> do
            rhs <- parseArgs xs
            return $ rhs { scDriver = WarpDriver 5000 }
        Just port -> do
            rhs <- parseArgs (drop 1 xs)
            return $ rhs { scDriver = WarpDriver port }
parseArgs' ("-cgi":xs) = do
    rhs <- parseArgs xs
    return $ rhs { scDriver = CGIDriver }
parseArgs' ("-scgi":xs) = do
    rhs <- parseArgs xs
    return $ rhs { scDriver = SCGIDriver }
parseArgs' ("-fcgi":xs) = do
    rhs <- parseArgs xs
    return $ rhs { scDriver = FastCGIDriver }
parseArgs' (x:xs) = do
    let portMay = readMaybe . unpack $ x
    case portMay of
        Nothing ->
            fail $ "Unknown argument: " ++ unpack x
        Just port -> do
            rhs <- parseArgs (drop 1 xs)
            return $ rhs { scDriver = WarpDriver port }

main :: IO ()
main = do
    hPutStrLn stderr ("Loading server configuration..." :: Text)
    args <- getArgs
    sconfig' <- loadServerConfig "."
    sconfig <- parseArgs args
    hPutStrLn stderr $ tshow sconfig

    hPutStrLn stderr ("Loading project..." :: Text)
    project <- loadProject sconfig "."
    hPutStrLn stderr ("OK" :: Text)
    serveProject sconfig project
