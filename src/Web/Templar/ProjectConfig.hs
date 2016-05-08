{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE TemplateHaskell #-}
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

data ProjectConfig =
    ProjectConfig
        { pcContextData :: HashMap Text BackendSpec
        , pcRules :: [Rule]
        }

instance Default ProjectConfig where
    def = ProjectConfig
            { pcContextData = mapFromList []
            , pcRules = []
            }

instance Monoid ProjectConfig where
    mempty = def
    mappend = pcAppend

instance FromJSON ProjectConfig where
    parseJSON (Object obj) = do
        contextData <- fromMaybe (mapFromList []) <$> obj .:? "data"
        rules <- fromMaybe [] <$> (obj .:? "rules" <|> obj .:? "Rules")
        return $ ProjectConfig
            { pcContextData = contextData
            , pcRules = rules
            }

pcAppend :: ProjectConfig -> ProjectConfig -> ProjectConfig
pcAppend a b =
    ProjectConfig
        { pcContextData = pcContextData a <> pcContextData b
        , pcRules = pcRules a <> pcRules b
        }

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
