{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.Replacement
( Replacement
, expandReplacement
, expandReplacementText
)
where

import ClassyPrelude
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Text.Ginger (Template, runGinger, parseGinger, makeContextText, toGVal)

data ReplacementItem =
    Literal Text |
    Variable Text
    deriving (Show, Eq)

newtype Replacement = Replacement Template
    deriving (Show)

instance FromJSON Replacement where
    parseJSON val = (maybe (fail "invalid replacement") return . parseReplacement) =<< parseJSON val

expandReplacementText :: HashMap Text Text -> Text -> Maybe Text
expandReplacementText variables input = do
    expandReplacement variables <$> parseReplacement input

parseReplacement :: Text -> Maybe Replacement
parseReplacement input =
    either (const Nothing) (Just . Replacement) . runIdentity $
    parseGinger
        (const $ return Nothing)
        Nothing
        (unpack input)

expandReplacement :: HashMap Text Text -> Replacement -> Text
expandReplacement variables (Replacement template) =
    runGinger context template
    where
        context = makeContextText lookupFn
        lookupFn varName = toGVal $ lookup varName variables
