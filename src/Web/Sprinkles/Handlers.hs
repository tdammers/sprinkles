{-#LANGUAGE NoImplicitPrelude #-}
module Web.Sprinkles.Handlers
( handleStaticTarget
, handleJSONTarget
, handleRedirectTarget
, handleTemplateTarget
, handleNotFound
, handleMethodNotAllowed
, handle404
, handle405
, handle500
)
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Handlers.Common
import Web.Sprinkles.Handlers.Static
import Web.Sprinkles.Handlers.Redirect
import Web.Sprinkles.Handlers.JSON
import Web.Sprinkles.Handlers.Template
