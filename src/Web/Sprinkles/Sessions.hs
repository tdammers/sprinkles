{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Web.Sprinkles.Sessions
( SessionHandle
, sessionID
, sessionGet
, sessionPut
, resumeSession
, newSession
, setSessionCookie
)
where

import ClassyPrelude
import Web.Sprinkles.SessionStore
import Web.Sprinkles.Logger as Logger
import Web.Sprinkles.Project
import Web.Sprinkles.ProjectConfig
import Web.Sprinkles.ServerConfig
import Network.Wai
import qualified Data.ByteString.Char8 as Char8
import Data.Char (isSpace)
import qualified Crypto.Nonce as Nonce
import Text.Printf (printf)

data SessionHandle =
    SessionHandle
        { sessionID :: SessionID
        , sessionGet :: Text -> IO (Maybe Text)
        , sessionPut :: Text -> Text -> IO ()
        }

setSessionCookie :: Project -> Request -> SessionHandle -> Response -> Response
setSessionCookie project request session =
    let sessionConfig = projectSessionConfig project
        cookieValue = mconcat [ (sessCookieName sessionConfig)
                              , "="
                              , (sessionID session)
                              ]
        cookieHeader = ("Set-Cookie", cookieValue)
    in mapResponseHeaders (cookieHeader:)


resumeSession :: Project -> Request -> IO (Maybe SessionHandle)
resumeSession project request = do
    let sessionConfig = projectSessionConfig project
        sessionStore = projectSessionStore project
    let cookieHeaderMay = lookup "Cookie" (requestHeaders request)
    maybe (return Nothing) (loadSession sessionStore) $
        cookieHeaderMay >>=
            parseCookieHeader >>=
            lookup (sessCookieName sessionConfig)

newSession :: Project -> Request -> IO (Maybe SessionHandle)
newSession project request = do
    ssid <- Nonce.new >>= Nonce.nonce128url
    let sessionConfig = projectSessionConfig project
        sessionStore = projectSessionStore project
    ssCreateSession sessionStore ssid NeverExpires
    return . Just $ makeSessionHandle sessionStore ssid

parseCookieHeader :: ByteString -> Maybe [(ByteString, ByteString)]
parseCookieHeader headerVal =
    let parts = map (Char8.dropWhile isSpace) $
                    Char8.split ';' headerVal
        splitOnce sep str =
            let (a, b) = Char8.break (== sep) str
            in (a, Char8.drop 1 b)
    in Just (map (splitOnce '=') parts)

loadSession :: SessionStore -> ByteString -> IO (Maybe SessionHandle)
loadSession ss ssid = do
    exists <- ssDoesSessionExist ss ssid
    if exists
        then
            return . Just $ makeSessionHandle ss ssid
        else
            return Nothing

makeSessionHandle :: SessionStore -> SessionID -> SessionHandle
makeSessionHandle ss ssid =
    SessionHandle
        { sessionID = ssid
        , sessionGet = ssGet ss ssid
        , sessionPut = ssPut ss ssid
        }
