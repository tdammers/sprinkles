{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | File backend loader
module Web.Sprinkles.Backends.Loader.RequestBodyLoader
( requestBodyLoader
)
where

import ClassyPrelude
import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        , rawFromLBS
        )
import Web.Sprinkles.Logger (LogLevel (..))
import Web.Sprinkles.Backends.Loader.Type
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
    contents <- rawFromLBS <$> loadPost pbs
    let meta = BackendMeta
                { bmMimeType = contentType pbs
                , bmMTime = Nothing
                , bmName = "POST"
                , bmPath = "POST"
                , bmSize = Nothing
                }
    return [BackendSource meta contents]
