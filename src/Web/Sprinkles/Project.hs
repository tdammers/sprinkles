{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Web.Sprinkles.Project
where

import Web.Sprinkles.Prelude hiding (readFile)
import Data.Aeson as JSON
import Text.Ginger
        ( parseGinger
        , Template
        , runGingerT
        , GingerContext
        , GVal (..)
        , ToGVal (..)
        , (~>)
        , SourcePos
        )
import Text.Ginger.Html (Html, htmlSource)
import qualified Text.Ginger as Ginger
import System.Directory (makeAbsolute, doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.FilePath
import System.IO (readFile)
import Data.Time.Clock.POSIX (POSIXTime)

import Web.Sprinkles.Exceptions
import Web.Sprinkles.Rule
import Web.Sprinkles.ProjectConfig
import Web.Sprinkles.ServerConfig
import Web.Sprinkles.Logger
import Web.Sprinkles.Cache
import Web.Sprinkles.Cache.Filesystem (filesystemCache)
import Web.Sprinkles.Cache.Memory (memCache)
import Web.Sprinkles.Cache.Memcached (memcachedCache)
import Web.Sprinkles.SessionStore
import Web.Sprinkles.SessionStore.Database (sqlSessionStore, DSN (..), SqlDriver (SqliteDriver))
import Web.Sprinkles.SessionStore.InProc (inProcSessionStore)

newtype TemplateCache = TemplateCache (HashMap Text (Template SourcePos))

data Project =
    Project
        { projectConfig :: ProjectConfig
        , projectTemplates :: TemplateCache
        , projectBackendCache :: Cache ByteString ByteString
        , projectLogger :: Logger
        , projectSessionStore :: SessionStore
        , projectSessionConfig :: SessionConfig
        }

loadProject :: ServerConfig -> IO Project
loadProject sconfig = do
    let dir = scRootDir sconfig
    pconfig <- loadProjectConfig dir
    logger <- createLogger $ fromMaybe (StdioLog Warning) (scLogger sconfig)
    templates <- preloadTemplates logger dir
    cache <- fmap mconcat
               . sequence
               . fmap (createCache dir)
               $ scBackendCache sconfig
    let sessionConfig = scSessions sconfig
    sessionStore <- createSessionStore sessionConfig
    return $ Project pconfig templates cache logger sessionStore sessionConfig

createSessionStore :: SessionConfig -> IO SessionStore
createSessionStore config = do
    case sessDriver config of
        SqlSessionDriver dsn -> sqlSessionStore dsn
        InProcSessionDriver -> inProcSessionStore
        _ -> return nullSessionStore

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
    isFile <- doesFileExist fp
    let isHidden = "." `isPrefixOf` (takeBaseName fp)
    return $ not isHidden && isFile

preloadTemplates :: Logger -> FilePath -> IO TemplateCache
preloadTemplates logger dir = do
    prefix <- makeAbsolute $ dir </> "templates"
    allFilenames <- findFilesR isTemplateFile prefix
    filenames <- findFiles isTemplateFile prefix
    templateSources <- forM allFilenames (readFile :: String -> IO String)
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

getTemplate :: Project -> Text -> IO (Template SourcePos)
getTemplate project templateName = do
    let TemplateCache tm = projectTemplates project
    maybe
        (throwM $ TemplateNotFoundException templateName)
        return
        (lookup templateName tm)


