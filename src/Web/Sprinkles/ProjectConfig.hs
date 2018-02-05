{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Web.Sprinkles.ProjectConfig
where

import ClassyPrelude
import Web.Sprinkles.Rule
import Data.Aeson as JSON
import Data.Aeson.TH
import qualified Data.Yaml as YAML
import Web.Sprinkles.Backends
import Data.Default
import System.FilePath.Glob (glob)
import System.Environment (getEnv, lookupEnv)
import Control.MaybeEitherMonad (maybeFail)
import Data.AList (AList)
import qualified Data.AList as AList
import Web.Sprinkles.Exceptions

data ProjectConfig =
    ProjectConfig
        { pcContextData :: AList Text BackendSpec
        , pcRules :: [Rule]
        }
        deriving (Show)

makeProjectPathsAbsolute :: FilePath -> ProjectConfig -> ProjectConfig
makeProjectPathsAbsolute dir (ProjectConfig context rules) =
  ProjectConfig (fmap goBackendSpec context) (fmap goRule rules)
  where
    goBackendSpec = makeBackendSpecPathsAbsolute dir
    goRule = makeRulePathsAbsolute dir

instance Default ProjectConfig where
    def = ProjectConfig
            { pcContextData = AList.empty
            , pcRules = []
            }

instance Monoid ProjectConfig where
    mempty = def
    mappend = pcAppend

instance FromJSON ProjectConfig where
    parseJSON = withObject "ProjectConfig" $ \obj -> do
        contextData <- fromMaybe AList.empty <$> obj .:? "data"
        rulesValue <- fromMaybe (toJSON ([] :: [Value])) <$> (obj .:? "rules" <|> obj .:? "Rules")
        rules <- parseJSON rulesValue
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
            (throwM . withSourceContext (pack fn))
            return

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir =
    fmap (makeProjectPathsAbsolute dir) . loadProjectConfigFile $ dir </> "project.yml"
