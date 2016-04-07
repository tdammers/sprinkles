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

data ProjectConfig =
    ProjectConfig
        { pcRules :: [Rule]
        }

$(deriveFromJSON defaultOptions { fieldLabelModifier = drop 2 } ''ProjectConfig)

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir = do
    YAML.decodeFileEither (dir </> "project.yml") >>=
        either
            (fail . show)
            return


