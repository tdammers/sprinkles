{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}

-- | CLI program that drives a Templar instance.
module Main where

import ClassyPrelude
import Web.Templar
import Text.Read (read, readMaybe)
import Data.Default (def)
import Text.Parsec

parseArgs :: [Text] -> IO ServerConfig
parseArgs argv = do
    let result = runParser argsP () "command line arguments" argv
    fns <- either
        (fail . show)
        return
        result
    return $ foldr ($) def fns

type ArgsP = Parsec [Text] ()

data ArgSpec a = Flag Text a
               | Optional Text (Maybe Text -> Maybe a)
               | Required Text (Text -> Maybe a)
               | Bare (Text -> Maybe a)

argsP :: ArgsP [ServerConfig -> ServerConfig]
argsP = Text.Parsec.many $ choice (map (Text.Parsec.try . argP) argSpecs)

tSatisfy :: (Show t, Stream s m t) => (t -> Bool) -> ParsecT s u m t
tSatisfy cond = do
    actual <- anyToken
    if cond actual
        then return actual
        else fail $ "unexpected " ++ show actual

tExactly :: (Show t, Stream s m t, Eq t) => t -> ParsecT s u m t
tExactly expected = tSatisfy (== expected)

isFlag :: Text -> Bool
isFlag = ("-" `isPrefixOf`)

isNotFlag :: Text -> Bool
isNotFlag = not . isFlag

argP :: ArgSpec a -> ArgsP a
argP (Flag str x) = do
    tExactly ("-" <> str)
    return x
argP (Optional str f) = do
    tExactly ("-" <> str)
    paramMay <- optionMaybe (tSatisfy isNotFlag)
    maybe
        (fail "invalid parameter")
        return
        (f paramMay)
argP (Required str f) = do
    tExactly ("-" <> str)
    param <- tSatisfy isNotFlag
    maybe
        (fail "invalid parameter")
        return
        (f param)
argP (Bare f) =
    (f <$> tSatisfy isNotFlag) >>= maybe (fail "invalid bare argument") return

argSpecs :: [ArgSpec (ServerConfig -> ServerConfig)]
argSpecs =
    [ Optional
        "warp"
        (maybe
            (Just $ \config -> config { scDriver = WarpDriver 5000 })
            (\str -> do
                port <- readMaybe . unpack $ str
                return $ \config -> config { scDriver = WarpDriver port })
        )
    , Bare (\str -> do
                port <- readMaybe . unpack $ str
                return $ \config -> config { scDriver = WarpDriver port })
    , Flag "cgi" (\config -> config { scDriver = CGIDriver })
    , Flag "scgi" (\config -> config { scDriver = SCGIDriver })
    , Flag "fcgi" (\config -> config { scDriver = FastCGIDriver })
    ]

main :: IO ()
main = do
    args <- getArgs
    sconfig' <- loadServerConfig "."
    sconfig <- parseArgs args

    project <- loadProject (sconfig' `mappend` sconfig) "."
    serveProject sconfig project
