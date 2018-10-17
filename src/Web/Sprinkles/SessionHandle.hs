{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE TypeApplications #-}
module Web.Sprinkles.SessionHandle
( SessionHandle (..)
, makeSessionHandle
)
where

import Web.Sprinkles.Prelude
import Web.Sprinkles.SessionStore
import Text.Ginger
       (GVal(..), ToGVal(..),
        (~>))
import qualified Text.Ginger as Ginger
import Data.Default

data SessionHandle =
    SessionHandle
        { sessionID :: SessionID
        , sessionGet :: Text -> IO (Maybe Text)
        , sessionPut :: Text -> Text -> IO ()
        }

instance (Monad m, MonadIO m) => ToGVal (Ginger.Run p m h) SessionHandle where
    toGVal session =
        Ginger.dict
            [ "id" ~> decodeUtf8 @Text (sessionID session)
            , ("get", Ginger.fromFunction (gSessionGet $ session))
            , ("put", Ginger.fromFunction (gSessionPut $ session))
            ]

gSessionGet :: (Monad m, MonadIO m) => SessionHandle -> Ginger.Function (Ginger.Run p m h)
gSessionGet session args = do
    let (matched, position, named) = Ginger.matchFuncArgs ["key"] args
    case lookup "key" matched of
        Nothing ->
            return def
        Just key -> do
            toGVal <$> liftIO (sessionGet session $ Ginger.asText key)

gSessionPut :: (Monad m, MonadIO m) => SessionHandle -> Ginger.Function (Ginger.Run p m h)
gSessionPut session args = do
    let (matched, position, named) = Ginger.matchFuncArgs ["key", "value"] args
    case lookup "key" matched of
        Nothing ->
            return def
        Just key -> do
            liftIO $ sessionPut session
                (Ginger.asText key)
                (Ginger.asText . fromMaybe def $ lookup "value" matched)
            return def

makeSessionHandle :: SessionStore -> SessionID -> SessionHandle
makeSessionHandle ss ssid =
    SessionHandle
        { sessionID = ssid
        , sessionGet = ssGet ss ssid
        , sessionPut = ssPut ss ssid
        }
