{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE RankNTypes #-}
module Web.Sprinkles.SessionStore.InProc
( inProcSessionStore
)
where

import Web.Sprinkles.Prelude hiding (atomically)
import Web.Sprinkles.SessionStore
import Control.Concurrent.STM

inProcSessionStore :: IO SessionStore
inProcSessionStore = do
    db <- newDB
    return SessionStore
        { ssGet = dbGet db
        , ssGetAll = dbGetAll db
        , ssList = dbList db
        , ssPut = dbPut db
        , ssCreateSession = dbCreate db
        , ssDropSession = dbDrop db
        , ssDoesSessionExist = dbExists db
        }

type DB = TVar (HashMap SessionID (TVar Session))

type Session = HashMap Text Text

newDB :: IO DB
newDB = newTVarIO $ mapFromList []

dbCreate :: DB -> SessionID -> SessionExpiry -> IO ()
dbCreate db ssid expiry = atomically $ do
    session <- newTVar $ mapFromList []
    modifyTVar db $ insertMap ssid session

dbDrop :: DB -> SessionID -> IO ()
dbDrop db ssid = atomically $
    modifyTVar db (deleteMap ssid)

dbExists :: DB -> SessionID -> IO Bool
dbExists db ssid = atomically $
    isJust <$> dbGetSession db ssid

dbGetSessionVar :: DB -> SessionID -> STM (Maybe (TVar Session))
dbGetSessionVar db ssid = lookup ssid <$> readTVar db

dbGetSession :: DB -> SessionID -> STM (Maybe Session)
dbGetSession db ssid = do
    dbGetSessionVar db ssid >>= maybe
        (return Nothing)
        (fmap Just . readTVar)

dbWithSession :: DB -> SessionID -> (Session -> STM (Maybe a)) -> (STM (Maybe a))
dbWithSession db ssid inner =
    dbGetSession db ssid >>= maybe
        (return Nothing)
        inner

dbGet :: DB -> SessionID -> Text -> IO (Maybe Text)
dbGet db ssid k = atomically $ do
    dbWithSession db ssid $ return . lookup k

dbGetAll :: DB -> SessionID -> IO [(Text, Text)]
dbGetAll db ssid = atomically $ do
    fmap (fromMaybe []) . dbWithSession db ssid $ return . Just . mapToList

dbList :: DB -> SessionID -> IO [Text]
dbList db ssid = atomically $ do
    fmap (fromMaybe []) . dbWithSession db ssid $ return . Just . keys

dbPut :: DB -> SessionID -> Text -> Text ->  IO ()
dbPut db ssid k v = atomically $ do
    dbGetSessionVar db ssid >>= maybe
        (throwM SessionNotFoundException)
        (flip modifyTVar $ insertMap k v)
