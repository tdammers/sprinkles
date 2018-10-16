{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE TupleSections #-}
module Web.Sprinkles.SessionStore
where

import Web.Sprinkles.Prelude
import Data.Time.Clock.POSIX

type SessionID = ByteString

data SessionExpiry = NeverExpires | Expires POSIXTime
        deriving (Show, Eq)

data SessionNotFoundException = SessionNotFoundException
    deriving (Show)

instance Exception SessionNotFoundException where

data SessionSupportDisabled = SessionSupportDisabled
    deriving (Show)

instance Exception SessionSupportDisabled where

-- | Common interface for session store backends.
data SessionStore =
    SessionStore
        { ssGet :: SessionID -> Text -> IO (Maybe Text)
        -- ^ Attempt to retrieve a value from a session
        , ssList :: SessionID -> IO [Text]
        -- ^ List all the keys in the session
        , ssGetAll :: SessionID -> IO [(Text,Text)]
        -- ^ Retrieve the entire session contents (key, value)
        , ssPut :: SessionID -> Text -> Text -> IO ()
        -- ^ Store a value in a session. Fail if session does not exist.
        , ssCreateSession :: SessionID -> SessionExpiry -> IO ()
        -- ^ Create a new session
        , ssDropSession :: SessionID -> IO ()
        -- ^ Drop a session and delete its associated data
        , ssDoesSessionExist :: SessionID -> IO Bool
        -- ^ Checks if a session exists

        -- , ssTouchSession :: SessionID -> IO ()
        -- -- ^ Shift a session's expiry timestamp to current time + expiry
        -- , ssVacuum :: IO ()
        -- -- ^ Drop all expired sessions
        }

nullSessionStore =
    SessionStore
        { ssGet = const . const $ return Nothing
        , ssList = const $ return []
        , ssGetAll = const $ return []
        , ssPut = const . const . const $ return ()
        , ssCreateSession = const . const $ throwM SessionSupportDisabled
        , ssDropSession = const $ return ()
        , ssDoesSessionExist = const $ return False
        }
