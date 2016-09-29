{-#LANGUAGE NoImplicitPrelude #-}
module Web.Templar.Handlers
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

import ClassyPrelude
import Web.Templar.Handlers.Common
import Web.Templar.Handlers.Static
import Web.Templar.Handlers.Redirect
import Web.Templar.Handlers.JSON
import Web.Templar.Handlers.Template
