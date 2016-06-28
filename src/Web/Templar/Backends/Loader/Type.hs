{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Backend loader type
module Web.Templar.Backends.Loader.Type
where

import ClassyPrelude

import Web.Templar.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        )
import Web.Templar.Backends.Spec
        ( FetchMode (..)
        , FetchOrder (..)
        )
import Web.Templar.Logger (LogLevel)

type Loader = (LogLevel -> Text -> IO ())
            -> FetchMode
            -> FetchOrder
            -> IO [BackendSource]
