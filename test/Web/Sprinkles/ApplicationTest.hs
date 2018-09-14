{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE NoImplicitPrelude #-}
module Web.Sprinkles.ApplicationTest
where

import ClassyPrelude
import Web.Sprinkles.Project (Project (..), loadProject)
import Web.Sprinkles.ServerConfig (ServerConfig (..))
import Web.Sprinkles.Serve (appFromProject)
import Web.Sprinkles.Logger (Logger (..), LogMessage (..), tChanLogger)
import System.Directory
import System.FilePath
import System.IO.Temp
import Data.Default (def)
import Test.Tasty
import Test.Tasty.HUnit
import Network.Wai.Test
import Text.Heredoc

applicationTests :: TestTree
applicationTests = testGroup "Application"
    [ testCase "Serve blank page" testServeBlankPage
    ]

testServeBlankPage = do
    let projectFiles =
            [ ( "project.yml"
              , [str|rules:
                    |  - pattern: '/'
                    |    template: 'index.html'
                    |])
            , ( "templates/index.html"
              , "All is well."
              )
            ]
    withFakeProject projectFiles . runProjectSession $ do
        response <- request (setPath defaultRequest "/")
        assertStatus 200 response
        assertContentType "text/html" response
        assertBody "All is well." response

runProjectSession :: Session a -> Project -> IO a
runProjectSession action project =
    runSession action (appFromProject project)

withFakeProject :: [(FilePath, ByteString)] -> (Project -> IO ()) -> IO ()
withFakeProject files inner = do
    withSystemTempDirectory "sprinkles-fake-project-" $ \projectRoot ->
        bracket (acquire projectRoot) release go
    where
        acquire :: FilePath -> IO FilePath
        acquire projectRoot = do
            setCurrentDirectory projectRoot
            forM files $ \(filename, contents) -> do
                let dirname = takeDirectory filename
                createDirectoryIfMissing True dirname
                writeFile filename contents
            return projectRoot

        release :: FilePath -> IO ()
        release projectRoot = return ()

        go :: FilePath -> IO ()
        go dir = do
            let sconfig = def { scRootDir = "." }
            logChan <- newTChanIO
            project <- setFakeLogger logChan <$> loadProject sconfig
            inner project

        setFakeLogger :: TChan LogMessage -> Project -> Project
        setFakeLogger logChan project =
            project { projectLogger = tChanLogger logChan }
