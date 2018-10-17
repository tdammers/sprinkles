{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TupleSections #-}
module Web.Sprinkles.Pattern
where

import Web.Sprinkles.Prelude hiding ( (<|>), Any, option )
import Text.Parsec as Parsec
import qualified Data.Text as Text
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Text.Regex.PCRE.String as RE
import Text.Regex.Base as RE
import qualified Data.Array as Array
import Control.MaybeEitherMonad
import System.IO.Unsafe (unsafePerformIO)
import Web.Sprinkles.MatchedText


data BasePatternItem =
    Exactly Text |
    Regex String RE.CompOption |
    Any
    deriving (Eq, Show)

data PatternPathItem =
    PatternPathItem
        { patternItemName :: Maybe Text
        , patternItemBase :: BasePatternItem
        , patternItemMatchMany :: MatchMulti
        }
    deriving (Eq, Show)

data MatchMulti = MatchOne | MatchMany
    deriving (Eq, Show, Enum, Bounded)

data PatternQueryItem =
    PatternQueryItem
        { patternQItemName :: Maybe Text
        , patternQItemKey :: Text
        , patternQItemValueBase :: BasePatternItem
        , patternQItemRequired :: Bool
        }
    deriving (Eq, Show)

data Pattern = Pattern [PatternPathItem] [PatternQueryItem]
    deriving (Eq, Show)

instance FromJSON Pattern where
    parseJSON val = (either error return . parsePattern) =<< parseJSON val

parsePattern :: Text -> Either String Pattern
parsePattern input =
    onLeft (\pe -> unpack input ++ "\n" ++ show pe) $
    runParser patternP () "" input

onLeft :: (a -> c) -> Either a b -> Either c b
onLeft f (Left x) = Left $ f x
onLeft _ (Right x) = Right x

patternP :: Parsec Text () Pattern
patternP = Pattern <$> patternPath <*> option [] patternQueryP

patternPath =
    some (Parsec.try patternPathItemP)
    <|> (string "/" >> return [])

patternPathItemP :: Parsec Text () PatternPathItem
patternPathItemP = do
    char '/'
    namedPathItemP <|> anonymousAnyP <|> anonymousLiteralP

patternQueryP :: Parsec Text () [PatternQueryItem]
patternQueryP = do
    char '?'
    patternQueryItemP `sepBy` char '&'

patternQueryItemP :: Parsec Text () PatternQueryItem
patternQueryItemP = do
    key <- pack <$> Parsec.many (noneOf ['=', '&'])
    char '='
    (nameMay, val) <- namedQueryItemP <|> fmap (Nothing,) baseItemP
    required <- option True $ char '?' *> return False
    return $ PatternQueryItem nameMay key val required

doubleBraced :: Parsec Text () a -> Parsec Text () a
doubleBraced inner =
    string "{{" *> inner <* string "}}"

nameP :: Parsec Text () Text
nameP = fmap pack . some $ alphaNum <|> oneOf "_-"

namedPathItemP :: Parsec Text () PatternPathItem
namedPathItemP = doubleBraced $ do
    name <- optionMaybe $ nameP <* char ':'
    base <- baseItemP
    multi <- multiModifierP
    return $ PatternPathItem name base multi

multiModifierP :: Parsec Text () MatchMulti
multiModifierP = (char '*' >> return MatchMany) <|> return MatchOne

namedQueryItemP :: Parsec Text () (Maybe Text, BasePatternItem)
namedQueryItemP = doubleBraced $ do
    name <- optionMaybe $ nameP <* char ':'
    base <- baseItemP
    return (name, base)

baseItemP :: Parsec Text () BasePatternItem
baseItemP = regexP <|> anyP <|> literalP

anyP :: Parsec Text () BasePatternItem
anyP = char '*' *> return Any

literalP :: Parsec Text () BasePatternItem
literalP = Exactly . pack <$> some (noneOf ['{', '}', '*', '/', '?', '&', '='])

regexP :: Parsec Text () BasePatternItem
regexP = do
    char '/'
    body <- Parsec.many regexCharP
    char '/'
    options <- sum <$> Parsec.many regexOptionP
    return $ Regex body options

regexCharP :: Parsec Text () Char
regexCharP = (Parsec.try $ char '\\' >> char '/') <|> noneOf "/"

regexOptionP :: Parsec Text () RE.CompOption
regexOptionP = (char 'm' >> return RE.compMultiline)
             <|> (char 'e' >> return RE.compExtended)
             <|> (char 'u' >> return RE.compUTF8)

anonymousLiteralP :: Parsec Text () PatternPathItem
anonymousLiteralP = PatternPathItem Nothing <$> literalP <*> multiModifierP

anonymousAnyP :: Parsec Text () PatternPathItem
anonymousAnyP = PatternPathItem Nothing <$> anyP <*> multiModifierP

matchPattern :: Pattern -> [Text] -> [(Text, Maybe Text)] -> Maybe (HashMap Text MatchedText)
matchPattern (Pattern pathItems queryItems) path query = do
    pathMatches <- matchPatternPath pathItems path
    queryMatches <- fmap MatchedText <$> matchPatternQuery queryItems query
    return $ pathMatches <> queryMatches

matchPatternPath :: [PatternPathItem] -> [Text] -> Maybe (HashMap Text MatchedText)
matchPatternPath [] []= Just (mapFromList [])
matchPatternPath [] _ = Nothing
matchPatternPath (x:xs) query = do
    (nameMay, value, remainder) <- matchPatternPathItem x query
    rest <- matchPatternPath xs remainder
    return $ case nameMay of
        Nothing -> rest
        Just name -> insertMap name value rest

matchPatternPathItem :: PatternPathItem -> [Text] -> Maybe (Maybe Text, MatchedText, [Text])
matchPatternPathItem (PatternPathItem nameMay base MatchOne) [] = Nothing
matchPatternPathItem (PatternPathItem nameMay base MatchOne) (query:remainder) = do
    value <- matchBaseItem base query
    return (nameMay, MatchedText value, remainder)
matchPatternPathItem (PatternPathItem nameMay base MatchMany) query = do
    let (values, remainder) = matchBaseItemMulti base query
    return (nameMay, MatchedTexts values, remainder)

matchPatternQuery :: [PatternQueryItem] -> [(Text, Maybe Text)] -> Maybe (HashMap Text Text)
matchPatternQuery items q = matchPatternQuery' items (fromMaybe "" <$> mapFromList q)

matchPatternQuery' :: [PatternQueryItem] -> HashMap Text Text -> Maybe (HashMap Text Text)
matchPatternQuery' [] _ = Just (mapFromList [])
matchPatternQuery' (x:xs) query = do
    (nameMay, value, remainder) <- matchPatternQueryItem x query
    rest <- matchPatternQuery' xs remainder
    return $ case nameMay of
        Nothing -> rest
        Just name -> insertMap name value rest

matchPatternQueryItem :: PatternQueryItem -> HashMap Text Text -> Maybe (Maybe Text, Text, HashMap Text Text)
matchPatternQueryItem (PatternQueryItem nameMay key valP required) query = do
    candidateValue <- lookup key query
    value <- matchBaseItem valP candidateValue
    return (nameMay, value, deleteMap key query)

matchBaseItemMulti :: BasePatternItem -> [Text] -> ([Text], [Text])
matchBaseItemMulti pitem [] = ([], [])
matchBaseItemMulti pitem (q:remainder) =
    case matchBaseItem pitem q of
        Nothing -> ([], remainder)
        Just value ->
            let (values, remainder') = matchBaseItemMulti pitem remainder
            in (value:values, remainder')

matchBaseItem :: BasePatternItem -> Text -> Maybe Text
matchBaseItem (Exactly t) x =
    if t == x
        then Just t
        else Nothing
matchBaseItem Any x = Just x
matchBaseItem (Regex body options) q = unsafePerformIO $ do
    re <- RE.compile options RE.execAnchored body >>= eitherFail
    matches <- RE.execute re (unpack q) >>= eitherFail
    case Array.elems <$> matches of
        Just ((offset, length):_) ->
            return . Just . take length . drop offset $ q
        _ -> return Nothing
