{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE RankNTypes #-}
module Web.Sprinkles.SessionStore.Database
( sqlSessionStore
, DSN (..)
, SqlDriver (..)
)
where

import ClassyPrelude
import qualified Database.HDBC as HDBC
import qualified Web.Sprinkles.Databases as DB
import Web.Sprinkles.Databases (DSN (..), SqlDriver (..))
import Web.Sprinkles.SessionStore
import Text.Heredoc (here)
import Database.YeshQL

sqlSessionStore :: DSN -> IO SessionStore
sqlSessionStore dsn = do
    conn <- sdbSetup dsn
    return SessionStore
        { ssGet = sdbGet conn
        , ssGetAll = sdbGetAll conn
        , ssList = sdbList conn
        , ssPut = sdbPut conn
        , ssCreateSession = sdbCreate conn
        , ssDropSession = sdbDrop conn
        , ssDoesSessionExist = sdbExists conn
        }

sdbSetup :: DSN -> IO HDBC.ConnWrapper
sdbSetup dsn = do
    conn <- DB.connect dsn
    HDBC.withTransaction conn $ \conn -> do
        HDBC.runRaw conn [here|
            CREATE TABLE IF NOT EXISTS sessions
                ( ssid VARCHAR NOT NULL
                ) |]
    HDBC.withTransaction conn $ \conn -> do
        HDBC.runRaw conn [here|
            CREATE TABLE IF NOT EXISTS session_data
                ( ssid VARCHAR NOT NULL
                , skey VARCHAR NOT NULL
                , sval VARCHAR NOT NULL
                , PRIMARY KEY (ssid, skey)
                , FOREIGN KEY (ssid)
                    REFERENCES sessions (ssid)
                    ON DELETE CASCADE
                    ON UPDATE CASCADE
                );
            |]
    return conn

[yesh|
    -- name:get :: (Text)
    -- :ssid :: ByteString
    -- :skey :: Text
    SELECT sval
        FROM session_data
        WHERE ssid = :ssid
            AND skey = :skey LIMIT 1

    ;;;

    -- name:getAll :: [(Text, Text)]
    -- :ssid :: ByteString
    SELECT skey, sval
        FROM session_data
        WHERE ssid = :ssid

    ;;; 

    -- name:listKeys :: [(Text)]
    -- :ssid :: ByteString
    SELECT skey
        FROM session_data
        WHERE ssid = :ssid

    ;;; 

    -- name:put :: rowcount Int
    -- :ssid :: ByteString
    -- :skey :: Text
    -- :sval :: Text
    INSERT INTO session_data (ssid, skey, sval)
        SELECT ssid, :skey, :sval
            FROM sessions WHERE ssid = :ssid

    ;;; 

    -- name:dropSession :: rowcount Int
    -- :ssid :: ByteString
    DELETE FROM sessions WHERE ssid = :ssid

    ;;;

    -- name:createSession :: rowcount Int
    -- :ssid :: ByteString
    INSERT INTO sessions (ssid)
        VALUES (:ssid)

    ;;;

    -- name:sessionExists :: (Bool)
    -- :ssid :: ByteString
    SELECT COUNT(1) FROM sessions WHERE ssid = :ssid
|]

withDB :: HDBC.ConnWrapper
       -> (HDBC.ConnWrapper -> IO a)
       -> IO a
withDB conn inner = do
    conn' <- HDBC.clone conn
    HDBC.withTransaction conn' inner

sdbGet :: HDBC.ConnWrapper -> SessionID -> Text -> IO (Maybe Text)
sdbGet conn ssid skey =
    withDB conn $ get ssid skey

sdbList :: HDBC.ConnWrapper -> SessionID -> IO [Text]
sdbList conn ssid =
    withDB conn $ listKeys ssid

sdbGetAll :: HDBC.ConnWrapper -> SessionID -> IO [(Text, Text)]
sdbGetAll conn ssid =
    withDB conn $ getAll ssid

sdbPut :: HDBC.ConnWrapper -> SessionID -> Text -> Text -> IO ()
sdbPut conn ssid skey sval =
    (withDB conn $ put ssid skey sval) >>= \case
        1 -> return ()
        0 -> throwM SessionNotFoundException
        _ -> error "More than one session with the same key. This is strange."

sdbCreate :: HDBC.ConnWrapper -> SessionID -> SessionExpiry -> IO ()
sdbCreate conn ssid expiry =
    (withDB conn $ createSession ssid) >>= \case
        1 -> return ()
        0 -> error "Session creation failed"
        _ -> error "More than one session created. This is strange."

sdbDrop :: HDBC.ConnWrapper -> SessionID -> IO ()
sdbDrop conn ssid =
    (withDB conn $ dropSession ssid) >> return ()

sdbExists :: HDBC.ConnWrapper -> SessionID -> IO Bool
sdbExists conn ssid =
    fmap (fromMaybe False) . withDB conn $ sessionExists ssid
