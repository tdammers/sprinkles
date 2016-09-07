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

data PostBodySource =
    PostBodySource
        { loadPost :: IO LByteString
        , contentType :: ByteString
        }

type Loader = (LogLevel -> Text -> IO ())
            -> PostBodySource
            -> FetchMode
            -> FetchOrder
            -> IO [BackendSource]
