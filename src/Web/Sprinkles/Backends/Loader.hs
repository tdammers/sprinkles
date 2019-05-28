{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TypeFamilies #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE CPP #-}

-- | Backend loader type
module Web.Sprinkles.Backends.Loader
( loader
)
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.Backends.Loader.Type
import Web.Sprinkles.Backends.Loader.SqlLoader
import Web.Sprinkles.Backends.Loader.SubprocessLoader
import Web.Sprinkles.Backends.Loader.FileLoader
#if FEATURE_CURL
import Web.Sprinkles.Backends.Loader.HttpLoader
#endif
import Web.Sprinkles.Backends.Loader.RequestBodyLoader
import Web.Sprinkles.Backends.Loader.LiteralLoader
import Web.Sprinkles.Backends.Spec
import Web.Sprinkles.Databases (ResultSetMode (..))

loader :: BackendType -> Loader
loader (SqlBackend dsn query params) = sqlLoader dsn ResultsLast [(query, params)]
loader (SqlMultiBackend dsn mode queries) = sqlLoader dsn mode queries
loader (FileBackend filepath) = fileLoader filepath
#if FEATURE_CURL
loader (HttpBackend uriText credentials) = curlLoader uriText credentials
#else
loader (HttpBackend uriText credentials) = error "HTTP backend support disabled"
#endif
loader (SubprocessBackend cmd args mimeType) = subprocessLoader cmd args mimeType
loader RequestBodyBackend = requestBodyLoader
loader (LiteralBackend body) = literalLoader body
