{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.Handlers.JSON
( handleJSONTarget
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
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import Data.Aeson.TH as JSON
import Data.AList (AList)
import qualified Data.AList as AList

handleJSONTarget :: AList Text BackendSpec
                 -> Set Text
                 -> Project
                 -> Wai.Application
handleJSONTarget backendPaths
                 required
                 project
                 request
                 respond = do
    let cache = projectBackendCache project
        logger = projectLogger project
    backendData <- loadBackendDict
                        (writeLog logger)
                        (pbsFromRequest request)
                        cache
                        backendPaths
                        required
    respond $ Wai.responseLBS
        status200
        [("Content-type", "application/json")]
        (JSON.encode backendData)


