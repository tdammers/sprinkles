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

data ReplacementItem =
    Literal Text |
    Variable Text
    deriving (Show, Eq)

newtype Replacement = Replacement [ReplacementItem]
    deriving (Show, Eq)

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
    name <- fmap pack (some alphaNum)
    char '}'
    return $ Variable name

literalP :: Parsec Text () ReplacementItem
literalP = Literal . pack <$> some (noneOf ['{', '}'])
