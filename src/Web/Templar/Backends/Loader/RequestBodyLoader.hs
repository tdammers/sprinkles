{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | File backend loader
module Web.Templar.Backends.Loader.RequestBodyLoader
( requestBodyLoader
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

requestBodyLoader :: Loader
requestBodyLoader writeLog pbs fetchMode fetchOrder = do
    contents <- loadPost pbs
    let meta = BackendMeta
                { bmMimeType = contentType pbs
                , bmMTime = Nothing
                , bmName = "POST"
                , bmPath = "POST"
                , bmSize = Nothing
                }
    return [BackendSource meta contents]
