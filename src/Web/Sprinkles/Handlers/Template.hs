{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Sprinkles.Handlers.Template
( handleTemplateTarget
)
where

import ClassyPrelude
import Web.Sprinkles.Backends
import qualified Network.Wai as Wai
import Web.Sprinkles.Logger as Logger
import Web.Sprinkles.Project
import Web.Sprinkles.ProjectConfig
import Web.Sprinkles.Handlers.Common
import Web.Sprinkles.Handlers.Respond
import Network.HTTP.Types
       (Status, status200, status302, status400, status404, status500)
import Web.Sprinkles.Backends.Loader.Type
       (PostBodySource (..), pbsFromRequest, pbsInvalid)
import Data.AList (AList)
import qualified Data.AList as AList

handleTemplateTarget :: Text -> ContextualHandler
handleTemplateTarget templateName
                     backendData
                     project
                     request
                     respond =
    respondTemplateHtml
        project
        status200
        templateName
        backendData
        request
        respond
