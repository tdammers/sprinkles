{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveGeneric #-}

module Web.Sprinkles.Exceptions
( formatException
, RequestValidationException (..)
, GingerFunctionCallException (..)
, InvalidReaderException (..)
, TemplateNotFoundException (..)
, handleUncaughtExceptions
, withSourceContext
, overrideSourceContext
)
where

import ClassyPrelude
import Control.Exception
import qualified Database.HDBC as HDBC
import Database.HDBC (SqlError (..))
import GHC.Generics
import Text.Pandoc.Error (PandocError (..))
import qualified Data.Yaml as YAML
import Text.Ginger as Ginger

-- * Various exception types for specific situations

data RequestValidationException =
    RequestValidationException
    deriving (Show, Eq, Generic)

instance Exception RequestValidationException

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

data TemplateNotFoundException = TemplateNotFoundException Text
    deriving (Show, Eq, Generic)

instance Exception TemplateNotFoundException

data SourceContextException =
    WithSourceContext { sourceContext ::  Text, withoutSourceContext :: SomeException }
    deriving (Show, Generic)

instance Exception SourceContextException where

class HasSourceContext a where
    overrideSourceContext :: Text -> a -> a

instance HasSourceContext SourceContextException where
    overrideSourceContext = \source e ->
        WithSourceContext source (withoutSourceContext e)

instance HasSourceContext Ginger.ParserError where
    overrideSourceContext = \s e -> e { peSourceName = Just . unpack $ s }

withSourceContext :: Exception e => Text -> e -> SomeException
withSourceContext context =
    toException . WithSourceContext context . toException

-- * Exception formatting

formatException :: SomeException -> Text
formatException e =
    fromMaybe (tshow e) . foldr (<|>) Nothing $ map ($ e) formatters

formatters :: [SomeException -> Maybe Text]
formatters =
    [ fmap formatSqlError . fromException
    , fmap formatIOError . fromException
    , fmap formatYamlParseException . fromException
    , fmap formatTemplateNotFoundException . fromException
    , fmap formatWithSourceContext . fromException
    , fmap formatGingerFunctionCallException . fromException
    , fmap formatGingerParserError . fromException
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

formatGingerParserError :: Ginger.ParserError -> Text
formatGingerParserError e =
    "line " <> (fromMaybe "?" . fmap tshow . Ginger.peSourceLine $ e)
    <> ", column " <> (fromMaybe "?" . fmap tshow . Ginger.peSourceColumn $ e)
    <> ": " <> pack (Ginger.peErrorMessage e)

formatYamlParseException :: YAML.ParseException -> Text
formatYamlParseException = pack . YAML.prettyPrintParseException

formatTemplateNotFoundException :: TemplateNotFoundException -> Text
formatTemplateNotFoundException (TemplateNotFoundException templateName) =
    "Template not found: " <> templateName

formatWithSourceContext :: SourceContextException -> Text
formatWithSourceContext (WithSourceContext context inner) =
    "In '" <> context <> "': " <> formatException inner

-- * Handling exceptions

handleUncaughtExceptions :: SomeException -> IO ()
handleUncaughtExceptions e =
    hPutStrLn stderr . formatException $ e
