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

import Web.Sprinkles.Prelude
import qualified Network.Wai as Wai
import Network.HTTP.Types (HeaderName)
import Web.Sprinkles.SessionHandle (SessionHandle)
import Debug.Trace (trace)

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
        , sessionHandle :: Maybe SessionHandle
        , lookupHeader :: HeaderName -> Maybe ByteString
        }

contentType :: RequestContext -> ByteString
contentType rqc = fromMaybe "text/plain" $ lookupHeader rqc "Content-type"

type Loader = (LogLevel -> Text -> IO ())
            -> RequestContext
            -> FetchMode
            -> FetchOrder
            -> IO [BackendSource]

pbsFromRequest :: Wai.Request -> Maybe SessionHandle -> RequestContext
pbsFromRequest request session =
    RequestContext
        { loadPost = Wai.lazyRequestBody request
        , sessionHandle = session
        , lookupHeader = \name ->
            lookup name (Wai.requestHeaders request)
        }

pbsInvalid :: RequestContext
pbsInvalid =
    RequestContext
        { loadPost = fail "POST body not available"
        , sessionHandle = Nothing
        , lookupHeader = const Nothing
        }

