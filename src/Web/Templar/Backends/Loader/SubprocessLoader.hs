{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Subprocess backend loader
module Web.Templar.Backends.Loader.SubprocessLoader
( subprocessLoader
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
import Web.Templar.Backends.Loader.Type
import Network.Mime (MimeType)
import Web.Templar.Logger (LogLevel (..))
import qualified System.Process as Process

subprocessLoader :: Text -> [Text] -> MimeType -> Loader
subprocessLoader cmd args mimeType writeLog _ fetchMode fetchOrder = do
    (_, Just hOut, _, _) <-
        Process.createProcess
            (Process.proc (unpack cmd) (map unpack args)) { Process.std_out = Process.CreatePipe }
    writeLog Debug $ tshow (cmd:args)
    body <- hGetContents hOut
    writeLog Debug $ tshow body
    let contentLength = fromIntegral $ length body
    let meta = BackendMeta
                { bmMimeType = mimeType
                , bmMTime = Nothing
                , bmName = cmd
                , bmPath = concat . intersperse " " $ cmd:args
                , bmSize = Just contentLength
                }
    return [BackendSource meta body]
