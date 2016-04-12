{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Templar.Pattern
where

import ClassyPrelude hiding ( (<|>) )
import Text.Parsec as Parsec
import qualified Data.Text as Text
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Text.Regex.PCRE.String as RE
import Text.Regex.Base as RE
import qualified Data.Array as Array
import Control.MaybeEitherMonad
import System.IO.Unsafe (unsafePerformIO)

data BasePatternItem =
    Exactly Text |
    Regex String RE.CompOption |
    AnyOne |
    Any
    deriving (Eq, Show)

data PatternItem =
    PatternItem
        { patternItemName :: Maybe Text
        , patternItemBase :: BasePatternItem
        }
    deriving (Eq, Show)

newtype Pattern = Pattern [PatternItem]
    deriving (Eq, Show)

instance FromJSON Pattern where
    parseJSON val = (maybe (fail "invalid pattern") return . parsePattern) =<< parseJSON val

parsePattern :: Text -> Maybe Pattern
parsePattern input =
    either (const Nothing) Just $
        runParser patternP () "" input

patternP :: Parsec Text () Pattern
patternP = Pattern <$> some patternItemP

patternItemP :: Parsec Text () PatternItem
patternItemP =
    namedP <|>
    anonymousAnyP <|>
    anonymousLiteralP

namedP :: Parsec Text () PatternItem
namedP = do
    char '{'
    char '{'
    name <- optionMaybe $ fmap pack (some alphaNum <* char ':')
    base <- regexP <|> anyP <|> literalP
    char '}'
    char '}'
    return $ PatternItem name base

anyP :: Parsec Text () BasePatternItem
anyP = do
    char '*'
    (char '*' >> return Any) <|> return AnyOne

literalP :: Parsec Text () BasePatternItem
literalP = Exactly . pack <$> some (noneOf ['{', '}', '*'])

regexP :: Parsec Text () BasePatternItem
regexP = do
    char '/'
    body <- Parsec.many regexCharP
    char '/'
    options <- sum <$> Parsec.many regexOptionP
    return $ Regex body options

regexCharP :: Parsec Text () Char
regexCharP = (char '\\' >> char '/') <|> noneOf "/"

regexOptionP :: Parsec Text () RE.CompOption
regexOptionP = (char 'm' >> return RE.compMultiline)
             <|> (char 'e' >> return RE.compExtended)
             <|> (char 'i' >> return RE.compCaseless)
             <|> (char 'u' >> return RE.compUTF8)

anonymousLiteralP = PatternItem Nothing <$> literalP

anonymousAnyP = PatternItem Nothing <$> anyP

matchPattern :: Pattern -> Text -> Maybe (HashMap Text Text)
matchPattern (Pattern []) "" = Just (mapFromList [])
matchPattern (Pattern []) _ = Nothing
matchPattern (Pattern (x:xs)) query = do
    (nameMay, value, remainder) <- matchPatternItem x query
    rest <- matchPattern (Pattern xs) remainder
    return $ case nameMay of
        Nothing -> rest
        Just name -> insertMap name value rest

matchPatternItem :: PatternItem -> Text -> Maybe (Maybe Text, Text, Text)
matchPatternItem (PatternItem nameMay base) query = do
    (value, remainder) <- matchBaseItem base query
    return (nameMay, value, remainder)

matchBaseItem :: BasePatternItem -> Text -> Maybe (Text, Text)
matchBaseItem (Exactly t) query =
    if (t `isPrefixOf` query)
        then Just (t, drop (length t) query)
        else Nothing
matchBaseItem AnyOne query =
    let t = takeWhile (not . (`elem` ['/', '?', '&'])) query
    in matchBaseItem (Exactly t) query
matchBaseItem Any query =
    matchBaseItem (Exactly query) query
matchBaseItem (Regex body options) query = unsafePerformIO $ do
    re <- RE.compile options RE.execAnchored body >>= eitherFail
    matches <- RE.execute re (unpack query) >>= eitherFail
    case Array.elems <$> matches of
        Just ((offset, length):_) ->
            return . Just $ splitAt length (drop offset query)
        _ -> return Nothing
