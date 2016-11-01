{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Sprinkles.Replacement
( Replacement
, expandReplacement
, expandReplacementText
)
where

import ClassyPrelude
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Text.Ginger ( Template
                   , runGinger
                   , parseGinger
                   , makeContextText
                   , ToGVal
                   , toGVal
                   , GVal (..)
                   , Run
                   )
import qualified Text.Ginger as Ginger
import Data.Default
import Control.Monad.Writer (Writer)
import Web.Sprinkles.Exceptions (formatException)

data ReplacementItem =
    Literal Text |
    Variable Text
    deriving (Show, Eq)

newtype Replacement = Replacement Template
    deriving (Show)

instance FromJSON Replacement where
    parseJSON val = (either (fail . unpack . formatException) return . parseReplacement) =<< parseJSON val

expandReplacementText :: HashMap Text (GVal (Run (Writer Text) Text))
                      -> Text
                      -> Either SomeException Text
expandReplacementText variables input =
    expandReplacement variables <$> parseReplacement input

parseReplacement :: Text -> Either SomeException Replacement
parseReplacement input =
    either (Left . toException) (Right . Replacement) . runIdentity $
    parseGinger
        (const $ return Nothing)
        Nothing
        (unpack input)

expandReplacement :: HashMap Text (GVal (Run (Writer Text) Text)) -> Replacement -> Text
expandReplacement variables (Replacement template) =
    runGinger context template
    where
        context = makeContextText lookupFn
        lookupFn varName = fromMaybe def $ lookup varName variables
