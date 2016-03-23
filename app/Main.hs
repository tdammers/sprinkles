{-#LANGUAGE NoImplicitPrelude #-}
module Main where

import ClassyPrelude
import Web.Templar

main :: IO ()
main = do
    let port = 5000
    hPutStrLn stderr "Loading project..."
    project <- loadProject "."
    hPutStrLn stderr "OK"
    hPutStrLn stderr $ "Running server on port " ++ show port
    serveProject project port
