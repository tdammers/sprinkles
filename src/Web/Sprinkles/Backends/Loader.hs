{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}

-- | Backend loader type
module Web.Sprinkles.Backends.Loader
( loader
)
where

import ClassyPrelude
import Web.Sprinkles.Backends.Loader.Type
import Web.Sprinkles.Backends.Loader.SqlLoader
import Web.Sprinkles.Backends.Loader.SubprocessLoader
import Web.Sprinkles.Backends.Loader.FileLoader
import Web.Sprinkles.Backends.Loader.HttpLoader
import Web.Sprinkles.Backends.Loader.RequestBodyLoader
import Web.Sprinkles.Backends.Loader.LiteralLoader
import Web.Sprinkles.Backends.Spec
import Web.Sprinkles.Databases (ResultSetMode (..))

loader :: BackendType -> Loader
loader (SqlBackend dsn query params) = sqlLoader dsn ResultsLast [(query, params)]
loader (SqlMultiBackend dsn mode queries) = sqlLoader dsn mode queries
loader (FileBackend filepath) = fileLoader filepath
loader (HttpBackend uriText credentials) = curlLoader uriText credentials
loader (SubprocessBackend cmd args mimeType) = subprocessLoader cmd args mimeType
loader RequestBodyBackend = requestBodyLoader
loader (LiteralBackend body) = literalLoader body
