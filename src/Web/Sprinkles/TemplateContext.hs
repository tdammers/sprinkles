{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Sprinkles.TemplateContext
where

import ClassyPrelude
import Text.Ginger
       (parseGinger, Template, runGingerT, GingerContext, GVal(..), ToGVal(..),
        (~>))
import Text.Ginger.Html
       (unsafeRawHtml, html)
import qualified Text.Ginger as Ginger
import qualified Data.Yaml as YAML
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode.Pretty as JSON
import Data.Default (Default, def)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Locale.Read (getLocale)
import qualified Text.Pandoc as Pandoc
import qualified Text.Pandoc.Readers.Creole as Pandoc
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import Data.ByteString.Builder (stringUtf8)
import qualified Network.Wai as Wai
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types.URI (queryToQueryText)
import qualified Crypto.BCrypt as BCrypt

import Web.Sprinkles.Backends
import Web.Sprinkles.Exceptions
import Web.Sprinkles.Logger as Logger
import Web.Sprinkles.Backends.Loader.Type
       (RequestContext (..), pbsFromRequest, pbsInvalid)
import Web.Sprinkles.SessionHandle
import Data.RandomString (randomStr)

import Text.Printf (printf)

sprinklesGingerContext :: RawBackendCache
                       -> Wai.Request
                       -> Maybe SessionHandle
                       -> Logger
                       -> IO (HashMap Text (GVal (Ginger.Run IO h)))
sprinklesGingerContext cache request session logger = do
    csrfTokenMay <- case session of
        Nothing -> return Nothing
        Just handle -> sessionGet handle "csrf"
    writeLog logger Debug . pack . printf "CSRF token: %s" . show $ csrfTokenMay
    let csrfTokenInput = case csrfTokenMay of
            Just token ->
                mconcat
                    [ unsafeRawHtml "<input type=\"hidden\" name=\"__form_token\" value=\""
                    , html token
                    , unsafeRawHtml "\"/>"
                    ]
            Nothing ->
                unsafeRawHtml "<!-- no form token defined -->"
    return $ mapFromList
        [ "request" ~> request
        , "session" ~> session
        , "formToken" ~> csrfTokenMay
        , "formTokenInput" ~> csrfTokenInput
        , ("load", Ginger.fromFunction (gfnLoadBackendData (writeLog logger) cache))
        ] <> baseGingerContext logger

baseGingerContext :: Logger
                  -> HashMap Text (GVal (Ginger.Run IO h))
baseGingerContext logger =
    mapFromList
        [ ("ellipse", Ginger.fromFunction gfnEllipse)
        , ("json", Ginger.fromFunction gfnJSON)
        , ("yaml", Ginger.fromFunction gfnYAML)
        , ("getlocale", Ginger.fromFunction (gfnGetLocale (writeLog logger)))
        , ("pandoc", Ginger.fromFunction (gfnPandoc (writeLog logger)))
        , ("markdown", Ginger.fromFunction (gfnPandocAlias "markdown" (writeLog logger)))
        , ("textile", Ginger.fromFunction (gfnPandocAlias "textile" (writeLog logger)))
        , ("rst", Ginger.fromFunction (gfnPandocAlias "rst" (writeLog logger)))
        , ("creole", Ginger.fromFunction (gfnPandocAlias "creole" (writeLog logger)))
        , ("bcrypt", gnsBCrypt)
        , ("randomStr", Ginger.fromFunction gfnRandomStr)
        ]

gnsBCrypt :: GVal (Ginger.Run IO h)
gnsBCrypt =
    Ginger.dict
        [ ("hash", Ginger.fromFunction gfnBCryptHash)
        , ("validate", Ginger.fromFunction gfnBCryptValidate)
        ]

gfnBCryptHash :: Ginger.Function (Ginger.Run IO h)
gfnBCryptHash args = do
    let argSpec :: [(Text, Ginger.GVal (Ginger.Run IO h))]
        argSpec = [ ("password", def)
                  , ("cost", toGVal (4 :: Int))
                  , ("algorithm", toGVal ("$2y$" :: Text))
                  ]
    case Ginger.extractArgsDefL argSpec args of
        Right [passwordG, costG, algorithmG] -> do
            let password = encodeUtf8 . Ginger.asText $ passwordG
                algorithm = encodeUtf8 . Ginger.asText $ algorithmG
            cost <- maybe
                        (throwM $ GingerInvalidFunctionArgs "bcrypt.hash" "int cost")
                        (return . ceiling)
                        (asNumber costG)
            let policy = BCrypt.HashingPolicy cost algorithm
            hash <- liftIO $ BCrypt.hashPasswordUsingPolicy policy password
            return . toGVal . fmap decodeUtf8 $ hash
        _ -> throwM $ GingerInvalidFunctionArgs "bcrypt.hash" "string password, int cost, string algorithm"

gfnRandomStr :: Ginger.Function (Ginger.Run IO h)
gfnRandomStr args = do
    let defaultAlphabet = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] :: String
        argSpec :: [(Text, Ginger.GVal (Ginger.Run IO h))]
        argSpec = [ ("length", toGVal (8 :: Int))
                  , ("alphabet", toGVal defaultAlphabet)
                  ]
    case Ginger.extractArgsDefL argSpec args of
        Right [lengthG, alphabetG] -> do
            desiredLength :: Int <- case fmap round . asNumber $ lengthG of
                    Nothing -> throwM $ GingerInvalidFunctionArgs "randomStr" "int length"
                    Just l -> return l
            let alphabet :: String
                alphabet = unpack . Ginger.asText $ alphabetG
            when (null alphabet)
                (throwM $ GingerInvalidFunctionArgs "randomStr" "alphabet too small")
            liftIO $ toGVal <$> randomStr alphabet desiredLength
        _ -> throwM $ GingerInvalidFunctionArgs "randomStr" "int length, string alphabet"

gfnBCryptValidate :: Ginger.Function (Ginger.Run IO h)
gfnBCryptValidate args = do
    let argSpec :: [(Text, Ginger.GVal (Ginger.Run IO h))]
        argSpec = [ ("hash", def)
                  , ("password", def)
                  ]
    case Ginger.extractArgsDefL argSpec args of
        Right [hashG, passwordG] -> do
            let hash = encodeUtf8 . Ginger.asText $ hashG
                password = encodeUtf8 . Ginger.asText $ passwordG
            return . toGVal $ BCrypt.validatePassword hash password
        _ -> throwM $ GingerInvalidFunctionArgs "bcrypt.validate" "string password, int cost, string algorithm"

gfnPandoc :: forall h. (LogLevel -> Text -> IO ()) -> Ginger.Function (Ginger.Run IO h)
gfnPandoc writeLog args = liftIO . catchToGinger writeLog $
    case Ginger.extractArgsDefL [("src", ""), ("reader", "markdown")] args of
        Right [src, readerName] -> toGVal <$> pandoc (Ginger.asText readerName) (Ginger.asText src)
        _ -> throwM $ GingerInvalidFunctionArgs "pandoc" "string src, string reader"

gfnPandocAlias :: forall h. Text -> (LogLevel -> Text -> IO ()) -> Ginger.Function (Ginger.Run IO h)
gfnPandocAlias readerName writeLog args = liftIO . catchToGinger writeLog $
    case Ginger.extractArgsDefL [("src", "")] args of
        Right [src] -> toGVal <$> pandoc readerName (Ginger.asText src)
        _ -> throwM $ GingerInvalidFunctionArgs "pandoc" "string src, string reader"

pandoc :: Text -> Text -> IO Pandoc.Pandoc
pandoc readerName src = do
    reader <- either
        (\err -> fail $ "Invalid reader: " ++ show err)
        return
        (getReader $ unpack readerName)
    let read = case reader of
            Pandoc.StringReader r -> r Pandoc.def . unpack
            Pandoc.ByteStringReader r -> fmap (fmap fst) . r Pandoc.def . encodeUtf8
    read (fromStrict src) >>= either
        (\err -> fail $ "Reading " ++ show readerName ++ " failed: " ++ show err)
        return
    where
        getReader "creole" = Right $ Pandoc.mkStringReader Pandoc.readCreole
        getReader readerName = Pandoc.getReader readerName

gfnGetLocale :: forall h. (LogLevel -> Text -> IO ()) -> Ginger.Function (Ginger.Run IO h)
gfnGetLocale writeLog args = liftIO . catchToGinger writeLog $
    case Ginger.extractArgsDefL [("category", "LC_TIME"), ("locale", "")] args of
        Right [gCat, gName] ->
            case (Ginger.asText gCat, Text.unpack . Ginger.asText $ gName) of
                ("LC_TIME", "") -> toGVal <$> getLocale Nothing
                ("LC_TIME", localeName) -> toGVal <$> getLocale (Just localeName)
                (cat, localeName) -> return def -- valid call, but category not implemented
        _ -> throwM $ GingerInvalidFunctionArgs "getlocale" "string category, string name"

gfnEllipse :: Ginger.Function (Ginger.Run IO h)
gfnEllipse [] = return def
gfnEllipse [(Nothing, str)] =
    gfnEllipse [(Nothing, str), (Nothing, toGVal (100 :: Int))]
gfnEllipse [(Nothing, str), (Nothing, len)] = do
    let txt = Ginger.asText str
        actualLen = ClassyPrelude.length txt
        targetLen = fromMaybe 100 $ ceiling <$> Ginger.asNumber len
        txt' = if actualLen + 3 > targetLen
                    then take (targetLen - 3) txt <> "..."
                    else txt
    return . toGVal $ txt'
gfnEllipse ((Nothing, str):xs) = do
    let len = fromMaybe (toGVal (100 :: Int)) $ lookup (Just "len") xs
    gfnEllipse [(Nothing, str), (Nothing, len)]
gfnEllipse xs = do
    let str = fromMaybe def $ lookup (Just "str") xs
    gfnEllipse $ (Nothing, str):xs

gfnJSON :: Ginger.Function (Ginger.Run IO h)
gfnJSON ((_, x):_) =
    return . toGVal . LUTF8.toString . JSON.encodePretty $ x
gfnJSON _ =
    return def

gfnYAML :: Ginger.Function (Ginger.Run IO h)
gfnYAML ((_, x):_) =
    return . toGVal . UTF8.toString . YAML.encode $ x
gfnYAML _ =
    return def

gfnLoadBackendData :: forall h. (LogLevel -> Text -> IO ()) -> RawBackendCache -> Ginger.Function (Ginger.Run IO h)
gfnLoadBackendData writeLog cache args =
    Ginger.dict <$> forM (zip [0..] args) loadPair
    where
        loadPair :: (Int, (Maybe Text, GVal (Ginger.Run IO h)))
                 -> Ginger.Run IO h (Text, GVal (Ginger.Run IO h))
        loadPair (index, (keyMay, gBackendURL)) = do
            let backendURL = Ginger.asText gBackendURL
            backendData :: Items (BackendData IO h) <- liftIO $
                loadBackendData writeLog pbsInvalid cache =<< parseBackendURI backendURL
            return
                ( fromMaybe (tshow index) keyMay
                , toGVal backendData
                )

catchToGinger :: forall h m. (LogLevel -> Text -> IO ())
              -> IO (GVal m)
              -> IO (GVal m)
catchToGinger writeLog action =
    action
        `catch` (\(e :: SomeException) -> do
            writeLog Logger.Error . formatException $ e
            return . toGVal $ False
        )

instance ToGVal m Wai.Request where
    toGVal rq =
        Ginger.orderedDict
            [ "httpVersion" ~> tshow (Wai.httpVersion rq)
            , "method" ~> decodeUtf8 (Wai.requestMethod rq)
            , "path" ~> decodeUtf8 (Wai.rawPathInfo rq)
            , "query" ~> decodeUtf8 (Wai.rawQueryString rq)
            , "pathInfo" ~> Wai.pathInfo rq
            , ( "queryInfo"
              , Ginger.orderedDict
                    [ (key, toGVal val)
                    | (key, val)
                    <- queryToQueryText (Wai.queryString rq)
                    ]
              )
            , ( "headers"
              , Ginger.orderedDict
                    [ (decodeCI n, toGVal $ decodeUtf8 v)
                    | (n, v)
                    <- Wai.requestHeaders rq
                    ]
              )
            ]

decodeCI :: CI.CI ByteString -> Text
decodeCI = decodeUtf8 . CI.original

