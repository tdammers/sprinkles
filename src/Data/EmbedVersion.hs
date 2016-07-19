{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Data.EmbedVersion
where

import Distribution.Version
import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List (intercalate, isPrefixOf)
import System.Process
import System.Exit
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (isSpace)
import Control.Exception


getPackageVersion :: FilePath -> IO Version
getPackageVersion cabalFileName = do
    gpd <- readPackageDescription silent cabalFileName
    return . pkgVersion . package. packageDescription $ gpd

getPackageVersionStr :: FilePath -> IO String
getPackageVersionStr = fmap formatPackageVersion . getPackageVersion

runGit :: [String] -> IO String
runGit args = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "git" args ""
    case exitCode of
        ExitSuccess -> return stdout
        e -> fail $ "git:" ++ stderr

data GitVersion = GitTag String String
                | GitCommit String
                | Unversioned

getGitVersionStr :: IO GitVersion
getGitVersionStr = do
    currentCommit <- runGit ["log", "-1", "--pretty=tformat:%H %d"]
    let (commitHash, refInfo) = break isSpace currentCommit
    let refStrs = map Text.strip
                . Text.splitOn ","
                . Text.dropAround (`elem` ("()" :: [Char]))
                . Text.strip
                . Text.pack
                $ refInfo
    return $ case filter ("tag: " `Text.isPrefixOf`) refStrs of
            (tagRef:_) -> GitTag (Text.unpack $ Text.drop 5 tagRef) commitHash
            [] -> GitCommit commitHash

haveGitModifications :: IO Bool
haveGitModifications = do
    output <- runGit ["status", "--porcelain"]
    let outLines = filter (not . null) . lines $ output
    return . not . null $ outLines

formatPackageVersion :: Version -> String
formatPackageVersion v =
    intercalate "." . map show . versionBranch $ v

embedPackageVersionStr :: FilePath -> Q Exp
embedPackageVersionStr fp = do
    gitVersion <- runIO $ getGitVersionStr `catch` \(err :: IOError) -> return Unversioned
    cabalVersion <- runIO (getPackageVersionStr fp)

    let version = case gitVersion of
            GitCommit c -> cabalVersion ++ "+git-" ++ c
            GitTag t c ->
                if (cabalVersion ++ "-") `isPrefixOf` t || cabalVersion == t
                    then t
                    else cabalVersion ++ " (" ++ t ++ ")"
            Unversioned -> cabalVersion
    modificationsStr <- runIO $ do
        modifications <- haveGitModifications `catch` \(err :: IOError) -> return False
        if modifications
            then return "+modifications"
            else return ""
    stringE $ version ++ modificationsStr
