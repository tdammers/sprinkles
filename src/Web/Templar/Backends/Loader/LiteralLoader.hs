{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | Literal backend loader
module Web.Templar.Backends.Loader.LiteralLoader
( literalLoader
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
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import Web.Templar.Backends.Loader.Type

literalLoader :: JSON.Value -> Loader
literalLoader body writeLog _ fetchMode fetchOrder = do
    let json = JSON.encode body
        meta = BackendMeta
            { bmMimeType = "application/json"
            , bmMTime = Nothing
            , bmName = "literal"
            , bmPath = "literal"
            , bmSize = Just . fromIntegral $ length json
            }
    return [ BackendSource meta json ]
