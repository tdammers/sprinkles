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

data ProjectConfig =
    ProjectConfig
        { pcContextData :: HashMap Text BackendSpec
        , pcRules :: [Rule]
        }

instance FromJSON ProjectConfig where
    parseJSON (Object obj) = do
        contextData <- fromMaybe (mapFromList []) <$> obj .:? "data"
        rules <- fromMaybe [] <$> (obj .:? "rules" <|> obj .:? "Rules")
        return $ ProjectConfig
            { pcContextData = contextData
            , pcRules = rules
            }

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir = do
    YAML.decodeFileEither (dir </> "project.yml") >>=
        either
            (fail . show)
            return


