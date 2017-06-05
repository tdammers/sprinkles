{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TemplateHaskell #-}
module Web.Sprinkles.Bake
where

import ClassyPrelude
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ( (</>), takeDirectory, replaceExtension )
import Control.Monad.State
import Control.Lens
import Control.Lens.TH (makeLenses)
import Text.Printf (printf)
import Network.HTTP.Types (Status (..), status200)
import Network.Wai.Test
import Network.Wai (Application, Request (..))
import qualified Network.Wai as Wai
import Web.Sprinkles.Serve (appFromProject)
import Web.Sprinkles.Project
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Char (ord)
import Text.HTML.TagSoup (parseTags, Tag (..), Attribute)
import qualified Data.CSS.Syntax.Tokens as CSS
import Data.FileEmbed (embedStringFile)

defHtaccess :: String
defHtaccess = $(embedStringFile "embedded/.htaccess")

data BakeState
    = BakeState
        { _bsTodo :: [FilePath]
        , _bsDone :: Set FilePath
        , _bsBasedir :: FilePath
        , _bsApp :: Application
        }

makeLenses ''BakeState

defBakeState :: BakeState
defBakeState = BakeState [] Set.empty "." defaultApplication

defaultApplication :: Application
defaultApplication rq respond =
    respond $
        Wai.responseLBS
            status200
            [("Content-type", "text/plain;charset=utf8")]
            "Hello, world!"

type Bake = StateT BakeState IO

bakeProject :: FilePath -> Project -> IO ()
bakeProject destDir project = do
    putStrLn $ "Baking project into " <> pack destDir
    createDirectoryIfMissing True destDir
    let app = appFromProject project
    runBake destDir entryPoints app $ do
        bakeHtaccess
        bake404
        bakeApp
    where
        entryPoints =
            [ "/"
            , "/sitemap.xml"
            , "/favicon.ico"
            , "/robots.txt"
            ]

runBake :: FilePath -> [FilePath] -> Application -> Bake a -> IO a
runBake baseDir entryPoints app a =
    evalStateT a $ defBakeState
        { _bsTodo = entryPoints
        , _bsBasedir = baseDir
        , _bsApp = app
        }

bakeHtaccess :: Bake ()
bakeHtaccess = do
    basedir <- use bsBasedir
    liftIO $ writeFile (basedir </> ".htaccess") defHtaccess

bakeApp :: Bake ()
bakeApp = do
    use bsTodo >>= \case
        [] ->
            return ()
        (current:rest) -> do
            bsTodo .= rest
            bakePath current
            bsDone %= Set.insert current
            bakeApp

bakePath :: FilePath -> Bake ()
bakePath fp = do
    done <- use bsDone
    unless (fp `Set.member` done) $
        bakePage CreateIndexHtml [200] fp (dropLeadingSlash fp)

data HtmlMappingMode = MapHtmlDirect | CreateIndexHtml

bake404 :: Bake ()
bake404 = do
    bakePage MapHtmlDirect [404] nonsensicalPath "_errors/404"
    where
        nonsensicalPath = "/123087408972309872109873012984709218371209847123" 

dropLeadingSlash :: FilePath -> FilePath
dropLeadingSlash = \case
    '/':x -> x
    x -> x

bakePage :: HtmlMappingMode -> [Int] -> FilePath -> FilePath -> Bake ()
bakePage htmlMode expectedStatuses fp fn = do
    app <- use bsApp
    basedir <- use bsBasedir
    let dstFile = basedir </> fn
        dstDir = takeDirectory dstFile
    let session = do
            let rq = setPath defaultRequest (fromString fp)
            request rq
    rp <- liftIO $ runSession session app
    let status = simpleStatus rp
    liftIO $ printf "GET %s %i %s\n" ("/" </> fp) (statusCode status) (decodeUtf8 $ statusMessage status)
    if statusCode status `elem` expectedStatuses
        then do
            let ty = fromMaybe "application/octet-stream" $ lookup "content-type" (simpleHeaders rp)
                rawTy = BS.takeWhile (/= fromIntegral (ord ';')) ty
                rawTySplit = BS.split (fromIntegral . ord $ '/') rawTy

            liftIO $ printf "%s\n" (decodeUtf8 ty)
            let (linkUrls, dstDir', dstFile') = case rawTySplit of
                    ["text", "html"] ->
                        let body = LBS.toStrict $ simpleBody rp
                            soup = parseTags (decodeUtf8 body)
                            linkUrls = map (fp </>) . map Text.unpack $ extractLinkedUrls soup
                        in case htmlMode of
                                CreateIndexHtml ->
                                    (linkUrls, dstFile, dstFile </> "index.html")
                                MapHtmlDirect ->
                                    (linkUrls, dstDir, replaceExtension dstFile "html")
                    [_, "css"] ->
                        let body = decodeUtf8 . LBS.toStrict $ simpleBody rp
                            tokens = either error id $ CSS.tokenize body
                            linkUrls = map (takeDirectory fp </>) . map Text.unpack $ extractCssUrls tokens
                        in (linkUrls, dstDir, dstFile)
                    _ ->
                        ([], dstDir, dstFile)
            liftIO $ do
                createDirectoryIfMissing True dstDir'
                LBS.writeFile dstFile' (simpleBody rp)
            bsTodo <>= linkUrls
        else do
            liftIO $ putStrLn "skip"

extractLinkedUrls :: [Tag Text] -> [Text]
extractLinkedUrls tags = filter isLocalUrl $ do
    tags >>= \case
        TagOpen "a" attrs -> do
            attrs >>= \case
                ("href", url) -> return url
                _ -> []
        TagOpen "link" attrs -> do
            attrs >>= \case
                ("href", url) -> return url
                _ -> []
        TagOpen "script" attrs -> do
            attrs >>= \case
                ("src", url) -> return url
                _ -> []
        TagOpen "img" attrs -> do
            attrs >>= \case
                ("src", url) -> return url
                _ -> []
        _ -> []

isLocalUrl :: Text -> Bool
isLocalUrl url = not
    (  ("//" `Text.isPrefixOf` url)
    || ("http://" `Text.isPrefixOf` url)
    || ("https://" `Text.isPrefixOf` url)
    )

extractCssUrls :: [CSS.Token] -> [Text]
extractCssUrls tokens = filter isLocalUrl $ go tokens
    where
        go [] = []
        go (CSS.Url url:xs) = url:go xs
        go (CSS.Function "url":CSS.String _ url:xs) = url:go xs
        go (x:xs) = go xs
