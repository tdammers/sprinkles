{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE TemplateHaskell #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE LambdaCase #-}
module Web.Templar
where

import ClassyPrelude
import Text.Ginger (parseGinger, Template)
import Data.Aeson as JSON
import Data.Aeson.TH as JSON
import Data.Yaml as YAML
import System.FilePath.Glob
import System.FilePath
import Data.Text (Text)
import qualified Data.Text as Text

data RoutePatternItem =
    LiteralComponent Text |
    NamedComponent Text |
    AnyComponent
    deriving (Ord, Eq, Show, Read)

$(deriveJSON defaultOptions ''RoutePatternItem)

newtype RoutePattern = RoutePattern [RoutePatternItem]
    deriving (Ord, Eq, Show, Read, ToJSON, FromJSON)

data TemplateRule =
    TemplateRule
        { trRoutePattern :: RoutePattern
        , trTemplate :: Text
        }
$(deriveJSON defaultOptions { fieldLabelModifier = drop 2 } ''TemplateRule)

type FrontendPath = [Text]

data ProjectConfig =
    ProjectConfig
        { pcTemplateRules :: [TemplateRule]
        }

$(deriveJSON defaultOptions { fieldLabelModifier = drop 2 } ''ProjectConfig)

loadProjectConfig :: FilePath -> IO ProjectConfig
loadProjectConfig dir = do
    YAML.decodeFile (dir </> "project.yml") >>=
        maybe
            (fail "Invalid YAML in project.yml, or project.yml not found")
            return

newtype TemplateCache = TemplateCache (HashMap Text Template)

preloadTemplates :: FilePath -> IO TemplateCache
preloadTemplates dir = do
    allFilenames <- glob $ dir </> "templates" </> "**" </> "*.html"
    filenames <- glob $ dir </> "templates" </> "*.html"
    templateSources <- forM allFilenames readFile
    let templateSourceMap :: HashMap String String
        templateSourceMap = mapFromList $ zip allFilenames templateSources
        resolver :: String -> IO (Maybe String)
        resolver name = return $ lookup name templateSourceMap
    print . keys $ templateSourceMap
    templates <- forM filenames $ \filename -> do
        source <- maybe
                    (fail $ "Source not found: " <> filename)
                    return
                    (lookup filename templateSourceMap)
        parseGinger resolver (Just filename) source >>= \case
            Left err -> fail . show $ err
            Right t -> return t
    return . TemplateCache . mapFromList $ zip (map pack filenames) templates

data Project =
    Project
        { projectConfig :: ProjectConfig
        , projectTemplates :: TemplateCache
        }

loadProject :: FilePath -> IO Project
loadProject dir =
    Project <$> loadProjectConfig dir <*> preloadTemplates dir
