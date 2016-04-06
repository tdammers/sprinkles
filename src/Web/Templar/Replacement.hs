{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.Replacement
where

import ClassyPrelude hiding ( (<|>) )
import Text.Parsec
import Data.Aeson as JSON
import Data.Aeson.TH as JSON

data ReplacementItem =
    Literal Text |
    Variable Text
    deriving (Show, Eq)

newtype Replacement = Replacement [ReplacementItem]
    deriving (Show, Eq)

instance FromJSON Replacement where
    parseJSON val = (maybe (fail "invalid replacement") return . parseReplacement) =<< parseJSON val

expandReplacement :: HashMap Text Text -> Replacement -> Text
expandReplacement variables (Replacement items) =
    mconcat . map (expandReplacementItem variables) $ items

expandReplacementItem :: HashMap Text Text -> ReplacementItem -> Text
expandReplacementItem variables (Literal t) = t
expandReplacementItem variables (Variable t) = fromMaybe "" $ lookup t variables

parseReplacement :: Text -> Maybe Replacement
parseReplacement input =
    either (const Nothing) Just $
    runParser replacementP () "" input

replacementP :: Parsec Text () Replacement
replacementP = Replacement <$> some replacementItemP

replacementItemP :: Parsec Text () ReplacementItem
replacementItemP =
    namedP <|>
    literalP

namedP :: Parsec Text () ReplacementItem
namedP = do
    char '{'
    char '{'
    name <- fmap pack (some alphaNum)
    char '}'
    char '}'
    return $ Variable name

literalP :: Parsec Text () ReplacementItem
literalP = Literal . pack <$> some (noneOf ['{', '}'])
