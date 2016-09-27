{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}

module Web.Templar.Exceptions
( formatException
, GingerFunctionCallException (..)
)
where

import ClassyPrelude
import Control.Exception
import qualified Database.HDBC as HDBC
import Database.HDBC (SqlError (..))
import GHC.Generics

data GingerFunctionCallException =
    GingerInvalidFunctionArgs
        { invalidFunctionName :: Text
        , invalidFunctionExpectedArgs :: Text
        }
    deriving (Show, Eq, Generic)

instance Exception GingerFunctionCallException


formatException :: SomeException -> Text
formatException e =
    fromMaybe (tshow e) . foldr (<|>) Nothing $ map ($ e) formatters

formatters :: [SomeException -> Maybe Text]
formatters =
    [ (formatSqlError =<<) . fromException
    , (formatIOError =<<) . fromException
    ]

formatSqlError :: SqlError -> Maybe Text
formatSqlError (SqlError { seErrorMsg = msg }) =
    return $ "SQL Error: " <> pack msg

formatIOError :: IOException -> Maybe Text
formatIOError e =
    return $ tshow e

formatGingerFunctionCallException :: GingerFunctionCallException -> Maybe Text
formatGingerFunctionCallException e =
    return $ "Invalid arguments to function '" <> invalidFunctionName e <> "', expected " <> invalidFunctionExpectedArgs e
