{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeApplications #-}
module Web.Sprinkles.Handlers.Template
( handleAutoTemplateTarget
, handleHtmlTemplateTarget
, handleTextTemplateTarget
)
where

import Web.Sprinkles.Prelude
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
       (RequestContext (..), pbsFromRequest, pbsInvalid)
import Data.AList (AList)
import qualified Data.AList as AList
import Web.Sprinkles.SessionHandle
import Text.Ginger.Html (Html)
import Text.Ginger.GVal (GVal, toGVal, asText)
import Text.Ginger.Run (Run)
import qualified Data.Text as Text

handleAutoTemplateTarget :: Text -> ContextualHandler
handleAutoTemplateTarget name
  | ".txt" `Text.isSuffixOf` name
  || ".txt.tpl" `Text.isSuffixOf` name
  = handleTextTemplateTarget name
  | otherwise
  = handleHtmlTemplateTarget name

handleTextTemplateTarget :: Text -> ContextualHandler
handleTextTemplateTarget templateName
                         backendData
                         project
                         session
                         request
                         respond =
    respondTemplateText
        project
        session
        status200
        templateName
        (fmap (fmap conv) backendData)
        request
        respond
    where
      conv :: forall p. BackendData p IO Html -> Text
      conv = asText @(Run p IO Html) . toGVal

handleHtmlTemplateTarget :: Text -> ContextualHandler
handleHtmlTemplateTarget templateName
                         backendData
                         project
                         session
                         request
                         respond =
    respondTemplateHtml
        project
        session
        status200
        templateName
        backendData
        request
        respond
