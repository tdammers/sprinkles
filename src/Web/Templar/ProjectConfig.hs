{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE LambdaCase #-}
module Web.Templar.ProjectConfig
where

import ClassyPrelude
import Web.Templar.Rule
import Data.Aeson as JSON
import Data.Aeson.TH
import qualified Data.Yaml as YAML
import Web.Templar.Backends
import Data.Default
import System.FilePath.Glob (glob)
import System.Environment (getEnv)
import Control.MaybeEitherMonad (maybeFail)

data BackendCacheConfig =
    FilesystemCache FilePath

instance FromJSON BackendCacheConfig where
    parseJSON (String str) = maybeFail $ backendCacheConfigFromString str
    parseJSON (Object obj) = do
        (obj .: "type") >>= \case
            "file" -> FilesystemCache <$> (obj .:? "dir" .!= ".cache")
            x -> fail $ "Invalid backend cache type: '" <> x

backendCacheConfigFromString :: Text -> Maybe BackendCacheConfig
backendCacheConfigFromString str = do
    case splitSeq ":" str of
        ["file", dir] -> return $ FilesystemCache (unpack dir)
        ["file"] -> return $ FilesystemCache ".cache"
        xs -> Nothing

data ProjectConfig =
    ProjectConfig
        { pcContextData :: HashMap Text BackendSpec
        , pcRules :: [Rule]
        , pcBackendCache :: [BackendCacheConfig]
        }

instance Default ProjectConfig where
    def = ProjectConfig
            { pcContextData = mapFromList []
            , pcRules = []
            , pcBackendCache = def
            }

instance Monoid ProjectConfig where
    mempty = def
    mappend = pcAppend

instance FromJSON ProjectConfig where
    parseJSON (Object obj) = do
        contextData <- fromMaybe (mapFromList []) <$> obj .:? "data"
        rules <- fromMaybe [] <$> (obj .:? "rules" <|> obj .:? "Rules")
        caches <- fromMaybe [] <$> obj .:? "backend-cache"
        return $ ProjectConfig
            { pcContextData = contextData
            , pcRules = rules
            , pcBackendCache = caches
            }

pcAppend :: ProjectConfig -> ProjectConfig -> ProjectConfig
pcAppend a b =
    ProjectConfig
        { pcContextData = pcContextData a <> pcContextData b
        , pcRules = pcRules a <> pcRules b
        , pcBackendCache =
            firstNonNull (pcBackendCache b) (pcBackendCache a)
        }

firstNonNull :: [a] -> [a] -> [a]
firstNonNull [] xs = xs
firstNonNull xs _ = xs

loadProjectConfigFile :: FilePath -> IO ProjectConfig
loadProjectConfigFile fn = do
    hPutStrLn stderr $ "Loading: " <> fn
    YAML.decodeFileEither fn >>=
        either
            (fail . show)
            return

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir = do
    systemGlobalFilenames <- glob "/etc/templar/*.yml"
    globalFilenames <- glob "/usr/local/etc/templar/*.yml"
    homeDir <- getEnv "HOME"
    userFilenames <- glob (homeDir </> ".config" </> "templar" </> "*.yml")
    localFilenames <- glob (dir </> "config" </> "*.yml")
    projectFilenames <- glob (dir </> "project.yml")
    let filenames = mconcat
            [ systemGlobalFilenames
            , globalFilenames
            , userFilenames
            , localFilenames
            , projectFilenames
            ]
    mconcat <$> forM filenames loadProjectConfigFile
