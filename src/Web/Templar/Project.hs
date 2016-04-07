{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Web.Templar.Project
where

import ClassyPrelude
import Web.Templar.Rule
import Data.Aeson as JSON
import Text.Ginger
        ( parseGinger
        , Template
        , runGingerT
        , GingerContext
        , GVal (..)
        , ToGVal (..)
        , (~>)
        )
import Text.Ginger.Html (Html, htmlSource)
import qualified Text.Ginger as Ginger
import System.Directory (makeAbsolute)
import System.FilePath.Glob (glob)
import System.FilePath

import Web.Templar.ProjectConfig

newtype TemplateCache = TemplateCache (HashMap Text Template)

preloadTemplates :: FilePath -> IO TemplateCache
preloadTemplates dir = do
    prefix <- makeAbsolute $ dir </> "templates"
    allFilenames <- glob $ prefix </> "**" </> "*.html"
    filenames <- glob $ prefix </> "*.html"
    templateSources <- forM allFilenames readFile
    let templateSourceMap :: HashMap String String
        templateSourceMap =
            mapFromList $
                zip
                    (map (makeRelative prefix) allFilenames)
                    templateSources
        resolver :: String -> IO (Maybe String)
        resolver name =
            let name' = makeRelative prefix . normalise $ prefix </> name
            in return $ lookup name' templateSourceMap
    print $ allFilenames
    print . keys $ templateSourceMap
    let relativeFilenames = map (makeRelative prefix) filenames
    templates <- forM relativeFilenames $ \filename -> do
        source <- maybe
                    (fail $ "Source not found: " <> filename)
                    return
                    (lookup filename templateSourceMap)
        parseGinger resolver (Just filename) source >>= \case
            Left err -> fail . show $ err
            Right t -> return t
    return . TemplateCache . mapFromList $ zip (map pack relativeFilenames) templates

data Project =
    Project
        { projectConfig :: ProjectConfig
        , projectTemplates :: TemplateCache
        }

loadProject :: FilePath -> IO Project
loadProject dir = do
    config <- loadProjectConfig dir
    templates <- preloadTemplates dir
    return $ Project config templates

resolveTemplateName :: Project -> [Text] -> IO Text
resolveTemplateName project path = do
    let TemplateCache tm = projectTemplates project
    -- return . concat . intersperse "/" $ path
    return "default.html"

resolveTemplate :: Project -> [Text] -> IO Template
resolveTemplate project path = do
    templateName <- resolveTemplateName project path
    getTemplate project templateName

getTemplate :: Project -> Text -> IO Template
getTemplate project templateName = do
    let TemplateCache tm = projectTemplates project
    maybe
        (fail $ "Template not found: " <> unpack templateName)
        return
        (lookup templateName tm)


