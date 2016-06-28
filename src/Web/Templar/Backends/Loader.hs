{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE DeriveGeneric #-}

-- | Backend loader type
module Web.Templar.Backends.Loader
( loader
)
where

import ClassyPrelude
import Web.Templar.Backends.Loader.Type
import Web.Templar.Backends.Loader.SqlLoader
import Web.Templar.Backends.Loader.SubprocessLoader
import Web.Templar.Backends.Loader.FileLoader
import Web.Templar.Backends.Loader.HttpLoader
import Web.Templar.Backends.Spec

loader :: BackendType -> Loader
loader (SqlBackend dsn query params) = sqlLoader dsn query params
loader (FileBackend filepath) = fileLoader filepath
loader (HttpBackend uriText credentials) = httpLoader uriText credentials
loader (SubprocessBackend cmd args mimeType) = subprocessLoader cmd args mimeType
