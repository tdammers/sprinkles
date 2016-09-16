{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.Handlers.Redirect
( handleRedirectTarget
)
where

import ClassyPrelude
import Web.Templar.Backends
import qualified Network.Wai as Wai
import Web.Templar.Logger as Logger
import Web.Templar.Project
import Web.Templar.ProjectConfig
import Web.Templar.Handlers.Common
import Network.HTTP.Types
       (Status, status200, status302, status400, status404, status500)
import Web.Templar.Backends.Loader.Type
       (PostBodySource (..), pbsFromRequest, pbsInvalid)
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.UTF8 as UTF8
import Data.AList (AList)

handleRedirectTarget :: Text
                     -> AList Text BackendSpec
                     -> Set Text
                     -> Project
                     -> Wai.Application
handleRedirectTarget redirectPath
                     backendPaths
                     required
                     project
                     request
                     respond =
    respond $ Wai.responseLBS
        status302
        [("Location", UTF8.fromString . unpack $ redirectPath)]
        ""

