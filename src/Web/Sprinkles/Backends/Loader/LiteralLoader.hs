{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | Literal backend loader
module Web.Sprinkles.Backends.Loader.LiteralLoader
( literalLoader
)
where

import ClassyPrelude
import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        )
import Web.Sprinkles.Logger (LogLevel (..))
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import Web.Sprinkles.Backends.Loader.Type

literalLoader :: JSON.Value -> Loader
literalLoader body writeLog _ fetchMode fetchOrder = do
    let (mimeType, rawBody) = case body of
            String txt -> ( "text/plain;charset=utf8"
                          , fromStrict (encodeUtf8 txt)
                          )
            _ -> ( "application/json"
                 , JSON.encode body
                 )
    let meta = BackendMeta
            { bmMimeType = mimeType
            , bmMTime = Nothing
            , bmName = "literal"
            , bmPath = "literal"
            , bmSize = Just . fromIntegral $ length rawBody
            }
    return [ BackendSource meta rawBody ]
