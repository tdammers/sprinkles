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
    sdbSetup dsn
    return SessionStore
        { ssGet = sdbGet dsn
        , ssGetAll = sdbGetAll dsn
        , ssList = sdbList dsn
        , ssPut = sdbPut dsn
        , ssCreateSession = sdbCreate dsn
        , ssDropSession = sdbDrop dsn
        , ssDoesSessionExist = sdbExists dsn
        }

sdbSetup :: DSN -> IO ()
sdbSetup dsn =
    DB.withConnection dsn $ \conn -> do
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

withDB :: DSN
       -> (HDBC.ConnWrapper -> IO a)
       -> IO a
withDB dsn inner =
    DB.withConnection dsn $ \conn ->
        (HDBC.withTransaction conn $ inner)

sdbGet :: DSN -> SessionID -> Text -> IO (Maybe Text)
sdbGet dsn ssid skey =
    withDB dsn $ get ssid skey

sdbList :: DSN -> SessionID -> IO [Text]
sdbList dsn ssid =
    withDB dsn $ listKeys ssid

sdbGetAll :: DSN -> SessionID -> IO [(Text, Text)]
sdbGetAll dsn ssid =
    withDB dsn $ getAll ssid

sdbPut :: DSN -> SessionID -> Text -> Text -> IO ()
sdbPut dsn ssid skey sval =
    (withDB dsn $ put ssid skey sval) >>= \case
        1 -> return ()
        0 -> throwM SessionNotFoundException
        _ -> error "More than one session with the same key. This is strange."

sdbCreate :: DSN -> SessionID -> SessionExpiry -> IO ()
sdbCreate dsn ssid expiry =
    (withDB dsn $ createSession ssid) >>= \case
        1 -> return ()
        0 -> error "Session creation failed"
        _ -> error "More than one session created. This is strange."

sdbDrop :: DSN -> SessionID -> IO ()
sdbDrop dsn ssid =
    (withDB dsn $ dropSession ssid) >> return ()

sdbExists :: DSN -> SessionID -> IO Bool
sdbExists dsn ssid =
    fmap (fromMaybe False) . withDB dsn $ sessionExists ssid
