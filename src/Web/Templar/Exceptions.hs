{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}

module Web.Templar.Exceptions
( formatException
, GingerFunctionCallException (..)
, InvalidReaderException (..)
, handleUncaughtExceptions
)
where

import ClassyPrelude
import Control.Exception
import qualified Database.HDBC as HDBC
import Database.HDBC (SqlError (..))
import GHC.Generics
import Text.Pandoc.Error (PandocError (..))
import qualified Data.Yaml as YAML

-- * Various exception types for specific situations

data GingerFunctionCallException =
    GingerInvalidFunctionArgs
        { invalidFunctionName :: Text
        , invalidFunctionExpectedArgs :: Text
        }
    deriving (Show, Eq, Generic)

instance Exception GingerFunctionCallException

data InvalidReaderException =
    InvalidReaderException
        { invalidReaderName :: Text
        , invalidReaderMessage :: Text
        }
    deriving (Show, Eq, Generic)

instance Exception InvalidReaderException

throwInvalidReaderException :: String -> String -> IO ()
throwInvalidReaderException name msg =
    throwM $ InvalidReaderException (pack name) (pack msg)

-- * Exception formatting

formatException :: SomeException -> Text
formatException e =
    fromMaybe (tshow e) . foldr (<|>) Nothing $ map ($ e) formatters

formatters :: [SomeException -> Maybe Text]
formatters =
    [ fmap formatSqlError . fromException
    , fmap formatIOError . fromException
    , fmap formatYamlParseException . fromException
    ]

formatSqlError :: SqlError -> Text
formatSqlError (SqlError { seErrorMsg = msg }) =
    "SQL Error: " <> pack msg

formatIOError :: IOException -> Text
formatIOError e =
    tshow e

formatGingerFunctionCallException :: GingerFunctionCallException -> Text
formatGingerFunctionCallException e =
    "Invalid arguments to function '" <> invalidFunctionName e <> "', expected " <> invalidFunctionExpectedArgs e

formatYamlParseException :: YAML.ParseException -> Text
formatYamlParseException = pack . YAML.prettyPrintParseException

handleUncaughtExceptions :: SomeException -> IO ()
handleUncaughtExceptions e =
    hPutStrLn stderr . formatException $ e
