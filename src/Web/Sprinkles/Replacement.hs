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
                   , runGingerT
                   , parseGinger
                   , makeContextTextM
                   , ToGVal
                   , toGVal
                   , GVal (..)
                   , Run
                   )
import qualified Text.Ginger as Ginger
import Data.Default
import Control.Monad.Writer (Writer)
import Web.Sprinkles.Exceptions (formatException)
import Data.Text.Lazy.Builder as TextBuilder

data ReplacementItem =
    Literal Text |
    Variable Text
    deriving (Show, Eq)

newtype Replacement = Replacement Template
    deriving (Show)

instance FromJSON Replacement where
    parseJSON val = (either (fail . unpack . formatException) return . parseReplacement) =<< parseJSON val

expandReplacementText :: HashMap Text (GVal (Run IO Text))
                      -> Text
                      -> IO Text
expandReplacementText variables input =
    expandReplacement variables =<< either throwM return (parseReplacement input)

parseReplacement :: Text -> Either SomeException Replacement
parseReplacement input =
    either (Left . toException) (Right . Replacement) . runIdentity $
    parseGinger
        (const $ return Nothing)
        Nothing
        (unpack input)

expandReplacement :: HashMap Text (GVal (Run IO Text)) -> Replacement -> IO Text
expandReplacement variables (Replacement template) = do
    output <- newIORef (TextBuilder.fromText "")
    let emit :: Text -> IO ()
        emit t = modifyIORef' output (<> TextBuilder.fromText t)
        context = makeContextTextM lookupFn emit
        lookupFn varName = return . fromMaybe def $ lookup varName variables
    runGingerT context template
    toStrict . TextBuilder.toLazyText <$> readIORef output
