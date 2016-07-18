module Data.EmbedVersion
where

import Distribution.Version
import Distribution.Verbosity
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List (intercalate)

getPackageVersion :: FilePath -> IO Version
getPackageVersion cabalFileName = do
    gpd <- readPackageDescription silent cabalFileName
    return . pkgVersion . package. packageDescription $ gpd

getPackageVersionStr :: FilePath -> IO String
getPackageVersionStr = fmap formatVersion . getPackageVersion

formatVersion :: Version -> String
formatVersion v =
    intercalate "." . map show . versionBranch $ v

embedPackageVersionStr :: FilePath -> Q Exp
embedPackageVersionStr fp =
    stringE =<< runIO (getPackageVersionStr fp)
