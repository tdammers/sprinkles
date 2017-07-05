{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | Subprocess backend loader
module Web.Sprinkles.Backends.Loader.SubprocessLoader
( subprocessLoader
)
where

import ClassyPrelude
import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Verification (..)
        , Items (..)
        , reduceItems
        , rawFromLBS
        )
import Web.Sprinkles.Backends.Loader.Type
import Network.Mime (MimeType)
import Web.Sprinkles.Logger (LogLevel (..))
import qualified System.Process as Process

subprocessLoader :: Text -> [Text] -> MimeType -> Loader
subprocessLoader cmd args mimeType writeLog _ fetchMode fetchOrder = do
    (_, Just hOut, _, _) <-
        Process.createProcess
            (Process.proc (unpack cmd) (map unpack args)) { Process.std_out = Process.CreatePipe }
    body <- hGetContents hOut
    let contentLength = fromIntegral $ length body
    let meta = BackendMeta
                { bmMimeType = mimeType
                , bmMTime = Nothing
                , bmName = cmd
                , bmPath = unwords $ cmd:args
                , bmSize = Just contentLength
                }
    return [BackendSource meta (rawFromLBS body) Trusted]
