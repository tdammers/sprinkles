{-#LANGUAGE NoImplicitPrelude #-}
module Web.Templar
( loadProject
, serveProject
, ServerConfig (..)
, loadServerConfig
, ServerDriver (..)
)
where

import ClassyPrelude
import Web.Templar.Project
import Web.Templar.ServerConfig
import Web.Templar.Serve

