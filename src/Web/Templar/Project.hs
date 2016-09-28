{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Web.Templar.Project
where

import ClassyPrelude
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
import System.Directory (makeAbsolute, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath
import Data.Time.Clock.POSIX (POSIXTime)

import Web.Templar.Exceptions
import Web.Templar.Rule
import Web.Templar.ProjectConfig
import Web.Templar.ServerConfig
import Web.Templar.Logger
import Web.Templar.Cache
import Web.Templar.Cache.Filesystem (filesystemCache)
import Web.Templar.Cache.Memory (memCache)
import Web.Templar.Cache.Memcached (memcachedCache)

newtype TemplateCache = TemplateCache (HashMap Text Template)

data Project =
    Project
        { projectConfig :: ProjectConfig
        , projectTemplates :: TemplateCache
        , projectBackendCache :: Cache ByteString ByteString
        , projectLogger :: Logger
        }

loadProject :: ServerConfig -> FilePath -> IO Project
loadProject sconfig dir = do
    pconfig <- loadProjectConfig dir
    templates <- preloadTemplates dir
    caches <- sequence $ fmap (createCache dir) (scBackendCache sconfig)
    let cache = mconcat caches
    logger <- createLogger $ fromMaybe (StdioLog Warning) (scLogger sconfig)
    return $ Project pconfig templates cache logger


createLogger :: LoggerConfig -> IO Logger
createLogger DiscardLog =
    return nullLogger
createLogger (Syslog level) =
    return $ syslogLogger level
createLogger (StdioLog level) =
    newBufferedLogger (stderrLogger level)

createCache :: FilePath -> BackendCacheConfig -> IO (Cache ByteString ByteString)
createCache cwd (FilesystemCache dir expiration) =
    return $ filesystemCache
        (unpack . decodeUtf8) -- "serialize" key
        (encodeUtf8 . pack) -- "unserialize" key
        hPut -- write value
        hGetContents -- read value
        (cwd </> dir)
        expiration
createCache _ (MemCache expiration) = memCache expiration
createCache _ MemcachedCache = memcachedCache

enumerateFiles :: FilePath -> IO [FilePath]
enumerateFiles dir = map (dir </>) <$> getDirectoryContents dir

findFiles :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
findFiles p dir = enumerateFiles dir >>= filterM p

findFilesR :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
findFilesR p dir = do
    localFiles <- findFiles p dir
    subdirs <- findFiles isInterestingSubdir dir
    childFiles <- concat <$> mapM (findFilesR p) subdirs
    return $ localFiles ++ childFiles
    where
        isInterestingSubdir :: FilePath -> IO Bool
        isInterestingSubdir dirname = do
            isDir <- doesDirectoryExist dirname
            return $ isDir && not (isHiddenFile dirname)

isHiddenFile :: FilePath -> Bool
isHiddenFile = ("." `isPrefixOf`) . takeFileName

isTemplateFile :: FilePath -> IO Bool
isTemplateFile fp = do
    let extensionMatches = takeExtension fp == ".html"
    isFile <- doesFileExist fp
    return $ extensionMatches && isFile

preloadTemplates :: FilePath -> IO TemplateCache
preloadTemplates dir = do
    prefix <- makeAbsolute $ dir </> "templates"
    allFilenames <- findFilesR isTemplateFile prefix
    filenames <- findFiles isTemplateFile prefix
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
    let relativeFilenames = map (makeRelative prefix) filenames
    templates <- forM relativeFilenames $ \filename -> do
        source <- maybe
                    (throwM . TemplateNotFoundException . pack $ filename)
                    return
                    (lookup filename templateSourceMap)
        parseGinger resolver (Just filename) source >>= \case
            Left err -> throwM $ withSourceContext (pack filename) err
            Right t -> return t
    return . TemplateCache . mapFromList $ zip (map pack relativeFilenames) templates

getTemplate :: Project -> Text -> IO Template
getTemplate project templateName = do
    let TemplateCache tm = projectTemplates project
    maybe
        (throwM $ TemplateNotFoundException templateName)
        return
        (lookup templateName tm)


