{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.Handlers.Template
( handleTemplateTarget
)
where

import ClassyPrelude
import Web.Templar.Backends
import qualified Network.Wai as Wai
import Web.Templar.Logger as Logger
import Web.Templar.Project
import Web.Templar.ProjectConfig
import Web.Templar.Handlers.Common
import Web.Templar.Handlers.Respond
import Network.HTTP.Types
       (Status, status200, status302, status400, status404, status500)
import Web.Templar.Backends.Loader.Type
       (PostBodySource (..), pbsFromRequest, pbsInvalid)
import Data.AList (AList)
import qualified Data.AList as AList

handleTemplateTarget :: Text
                     -> AList Text BackendSpec
                     -> Set Text
                     -> Project
                     -> Wai.Application
handleTemplateTarget templateName
                     backendPaths
                     required
                     project
                     request
                     respond = do
    let cache = projectBackendCache project
        logger = projectLogger project
        go = do
            backendData <- loadBackendDict
                                (writeLog logger)
                                (pbsFromRequest request)
                                cache
                                backendPaths
                                required
            respondTemplateHtml
                project
                status200
                templateName
                backendData
                request
                respond
    go `catch` handleNotFound project request respond

