{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Sprinkles.Handlers.Redirect
( handleRedirectTarget
)
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Backends
import qualified Network.Wai as Wai
import Web.Sprinkles.Logger as Logger
import Web.Sprinkles.Project
import Web.Sprinkles.ProjectConfig
import Web.Sprinkles.Handlers.Common
import Network.HTTP.Types
       (Status, status200, status302, status400, status404, status500)
import Web.Sprinkles.Backends.Loader.Type
       (RequestContext (..), pbsFromRequest, pbsInvalid)
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.AList (AList)

handleRedirectTarget :: Text -> ContextualHandler
handleRedirectTarget redirectPath
                     backendData
                     project
                     session
                     request
                     respond =
    respond $ Wai.responseLBS
        status302
        [("Location", UTF8.fromString . unpack $ redirectPath)]
        ""

