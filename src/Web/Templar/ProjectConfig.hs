{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
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
import System.Environment (getEnv, lookupEnv)
import Control.MaybeEitherMonad (maybeFail)

data ProjectConfig =
    ProjectConfig
        { pcContextData :: HashMap Text BackendSpec
        , pcRules :: [Rule]
        }
        deriving (Show)

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
        return ProjectConfig
            { pcContextData = contextData
            , pcRules = rules
            }

pcAppend :: ProjectConfig -> ProjectConfig -> ProjectConfig
pcAppend a b =
    ProjectConfig
        { pcContextData = pcContextData a <> pcContextData b
        , pcRules = pcRules a <> pcRules b
        }

firstNonNull :: [a] -> [a] -> [a]
firstNonNull [] xs = xs
firstNonNull xs _ = xs

loadProjectConfigFile :: FilePath -> IO ProjectConfig
loadProjectConfigFile fn =
    YAML.decodeFileEither fn >>=
        either
            (fail . show)
            return

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir =
    loadProjectConfigFile $ dir </> "project.yml"

