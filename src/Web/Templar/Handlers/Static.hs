{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.Handlers.Static
( handleStaticTarget
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
import Data.AList (AList)
import qualified Data.AList as AList

handleStaticTarget :: ContextualHandler
handleStaticTarget backendData
                   project
                   request
                   respond = do
    let go = do
            backendItem <- case lookup "file" backendData of
                Nothing -> throwM NotFoundException
                Just NotFound -> throwM NotFoundException
                Just (SingleItem item) -> return item
                Just (MultiItem []) -> throwM NotFoundException
                Just (MultiItem (x:xs)) -> return x
            respond $ Wai.responseLBS
                status200
                [("Content-type", bmMimeType . bdMeta $ backendItem)]
                (bdRaw backendItem)
    go `catch` handleNotFound project request respond

