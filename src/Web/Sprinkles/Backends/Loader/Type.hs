{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | Backend loader type
module Web.Sprinkles.Backends.Loader.Type
where

import ClassyPrelude
import qualified Network.Wai as Wai
import Web.Sprinkles.SessionHandle (SessionHandle)

import Web.Sprinkles.Backends.Data
        ( BackendData (..)
        , BackendMeta (..)
        , BackendSource (..)
        , Items (..)
        , reduceItems
        )
import Web.Sprinkles.Backends.Spec
        ( FetchMode (..)
        , FetchOrder (..)
        )
import Web.Sprinkles.Logger (LogLevel)

data RequestContext =
    RequestContext
        { loadPost :: IO LByteString
        , contentType :: ByteString
        , getSessionHandle :: IO (Maybe SessionHandle)
        }

type Loader = (LogLevel -> Text -> IO ())
            -> RequestContext
            -> FetchMode
            -> FetchOrder
            -> IO [BackendSource]

pbsFromRequest :: Wai.Request -> IO (Maybe SessionHandle) -> RequestContext
pbsFromRequest request getSessionHandle =
    RequestContext
        { loadPost = Wai.lazyRequestBody request
        , contentType = fromMaybe "text/plain" $
            lookup "Content-type" (Wai.requestHeaders request)
        , getSessionHandle = getSessionHandle
        }

pbsInvalid :: RequestContext
pbsInvalid =
    RequestContext
        { loadPost = fail "POST body not available"
        , contentType = "text/plain"
        , getSessionHandle = return Nothing
        }

