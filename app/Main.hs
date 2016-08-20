{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE ScopedTypeVariables #-}

-- | CLI program that drives a Templar instance.
module Main where

import ClassyPrelude hiding ( (<|>), try )
import Web.Templar
import Text.Read (read, readMaybe)
import Data.Default (def)
import Text.Parsec
import Data.EmbedVersion

data CliOptions =
    ServeProject ServerConfig |
    DumpVersion

parseArgs :: [Text] -> IO CliOptions
parseArgs argv = do
    let result = runParser argsP () "command line arguments" argv
    either
        (fail . show)
        return
        result

type ArgsP = Parsec [Text] ()

data ArgSpec a = Flag Text a
               | Optional Text (Maybe Text -> Maybe a)
               | Required Text (Text -> Maybe a)
               | Bare (Text -> Maybe a)

argsP :: ArgsP CliOptions
argsP = versionP <|> serveArgsP

versionP :: ArgsP CliOptions
versionP =
    (try (tExactly "-version") <|>
     try (tExactly "--version") <|>
     try (tExactly "-v")) >> return DumpVersion

serveArgsP :: ArgsP CliOptions
serveArgsP = do
    pipeline <- Text.Parsec.many $ choice (map (Text.Parsec.try . argP) argSpecs)
    return . ServeProject $ foldr ($) def pipeline

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
            (Just $ \config -> config { scDriver = WarpDriver Nothing })
            (\str -> do
                port <- readMaybe . unpack $ str
                return $ \config -> config { scDriver = WarpDriver (Just port) })
        )
    , Bare (\str -> do
                port <- readMaybe . unpack $ str
                return $ \config -> config { scDriver = WarpDriver (Just port) })
    , Flag "cgi" (\config -> config { scDriver = CGIDriver })
    , Flag "scgi" (\config -> config { scDriver = SCGIDriver })
    , Flag "fcgi" (\config -> config { scDriver = FastCGIDriver })
    ]

templarVersion = $(embedPackageVersionStr "templar.cabal")

main :: IO ()
main = do
    args <- getArgs
    opts <- parseArgs args
    case opts of
        ServeProject sconfigA -> do
            sconfigF <- loadServerConfig "."
            let sconfig = sconfigF `mappend` sconfigA

            project <- loadProject sconfig "."
            serveProject sconfig project
        DumpVersion -> do
            putStrLn templarVersion
