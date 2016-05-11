{-#LANGUAGE NoImplicitPrelude #-}

-- | CLI program that drives a Templar instance.
module Main where

import ClassyPrelude
import Web.Templar
import Text.Read (read, readMaybe)

main :: IO ()
main = do
    args <- getArgs
    let port = fromMaybe 5000 $ readMaybe . unpack =<< headMay args
    hPutStrLn stderr "Loading project..."
    project <- loadProject "."
    hPutStrLn stderr "OK"
    hPutStrLn stderr $ "Running server on port " ++ show port
    serveProject project port
