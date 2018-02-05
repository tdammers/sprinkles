{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE LambdaCase #-}

-- | CLI program that drives a Sprinkles instance.
module Main where

import ClassyPrelude hiding ( (<|>), try )
import Web.Sprinkles
import Web.Sprinkles.Project (Project)
import Web.Sprinkles.Exceptions
import Text.Read (read, readMaybe)
import Data.Default (def)
import Text.Parsec
import Data.EmbedVersion
import qualified Data.Text as Text
import Control.Concurrent.Async

data CliOptions =
    ServeProject ServerConfig |
    BakeProject FilePath ServerConfig |
    DumpVersion
    deriving (Show)

parseArgs :: [Text] -> IO [CliOptions]
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

argsP :: ArgsP [CliOptions]
argsP = some (versionP <|> bakeArgsP <|> serveArgsP) <* eof

versionP :: ArgsP CliOptions
versionP =
    (try (tExactly "-version") <|>
     try (tExactly "--version") <|>
     try (tExactly "-v")) >> return DumpVersion

serveArgsP :: ArgsP CliOptions
serveArgsP = do
    try $ tExactly "-serve"
    pipeline <- Text.Parsec.many $ choice (map (Text.Parsec.try . serveArgP) serveArgSpecs)
    return . ServeProject $ foldr ($) def pipeline

bakeArgsP :: ArgsP CliOptions
bakeArgsP = do
    try $ tExactly "-bake"
    dirname <- Text.unpack <$> option "./baked" bareArgP
    return $ BakeProject dirname def

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

serveArgP :: ArgSpec a -> ArgsP a
serveArgP (Flag str x) = do
    tExactly ("-" <> str)
    return x
serveArgP (Optional str f) = do
    tExactly ("-" <> str)
    paramMay <- optionMaybe bareArgP
    maybe
        (fail "invalid parameter")
        return
        (f paramMay)
serveArgP (Required str f) = do
    tExactly ("-" <> str)
    param <- bareArgP
    maybe
        (fail "invalid parameter")
        return
        (f param)
serveArgP (Bare f) =
    (f <$> bareArgP) >>= maybe (fail "invalid bare argument") return

bareArgP :: ArgsP Text
bareArgP = tSatisfy isNotFlag

serveArgSpecs :: [ArgSpec (ServerConfig -> ServerConfig)]
serveArgSpecs =
    [ Optional
        "warp"
        (maybe
            (Just $ \config -> config { scDriver = WarpDriver Nothing })
            (\str -> do
                port <- readMaybe . unpack $ str
                return $ \config -> config { scDriver = WarpDriver (Just port) })
        )
    , Required
        "dir"
        (\str -> Just (\config -> config { scRootDir = unpack str }))
    , Bare (\str -> do
                port <- readMaybe . unpack $ str
                return $ \config -> config { scDriver = WarpDriver (Just port) })
    , Flag "cgi" (\config -> config { scDriver = CGIDriver })
    , Flag "scgi" (\config -> config { scDriver = SCGIDriver })
    , Flag "fcgi" (\config -> config { scDriver = FastCGIDriver })
    ]

sprinklesVersion = $(embedPackageVersionStr "sprinkles.cabal")

main :: IO ()
main = runMain `catch` handleUncaughtExceptions

prepareProject :: ServerConfig -> IO (ServerConfig, Project)
prepareProject sconfigA = do
    sconfigF <- loadServerConfig $ scRootDir sconfigA
    let sconfig = sconfigF `mappend` sconfigA

    project <- loadProject sconfig
    return (sconfig, project)

runMain :: IO ()
runMain = do
    args <- getArgs
    opts <- parseArgs args
    forConcurrently_ opts $ \opt -> do
      print opt
      case opt of
        ServeProject sconfigA -> do
            prepareProject sconfigA >>= \(sconfig, project) ->
                serveProject sconfig project
        BakeProject path sconfigA -> do
            prepareProject sconfigA >>= \(sconfig, project) ->
                bakeProject path project
        DumpVersion -> do
            putStrLn sprinklesVersion
