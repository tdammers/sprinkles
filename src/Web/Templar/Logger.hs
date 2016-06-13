{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Templar.Logger
( stderrLogger
, newBufferedLogger
, Logger (..)
, LogLevel (..)
, writeLog
)
where

import ClassyPrelude
import Control.Concurrent (forkIO)
import Data.Aeson (FromJSON (..), Value (..), (.:))

data LogLevel = Debug
              | Notice
              | Warning
              | Error
              | Critical
              deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON LogLevel where
    parseJSON (String "debug") = return Debug
    parseJSON (String "notice") = return Notice
    parseJSON (String "warning") = return Warning
    parseJSON (String "error") = return Error
    parseJSON (String "critical") = return Critical
    parseJSON x = fail $ "Invalid log level: " <> show x

data LogMessage =
    LogMessage
        { lmTimestamp :: UTCTime
        , lmLevel :: LogLevel
        , lmMessage :: Text
        }
        deriving (Show, Eq)

instance FromJSON LogMessage where
    parseJSON (Object o) =
        LogMessage <$> o .: "timestamp"
                   <*> o .: "level"
                   <*> o .: "message"

writeLogMessage :: Logger -> LogMessage -> IO ()
writeLogMessage logger message = do
    let messageLevel = Just $ lmLevel message
        loggingLevel = logLevel logger
    when
        (messageLevel >= loggingLevel)
        (writeLogRaw logger . formatMessage $ message)

writeLog :: Logger -> LogLevel -> Text -> IO ()
writeLog logger level message = do
    now <- getCurrentTime
    writeLogMessage logger $
        LogMessage now level message

formatMessage :: LogMessage -> Text
formatMessage msg =
    (tshow $ lmTimestamp msg) <>
    " [" <>
    (tshow $ lmLevel msg) <>
    "] " <>
    (lmMessage msg)

data Logger =
    Logger
        { writeLogRaw :: Text -> IO ()
        , logLevel :: Maybe LogLevel
        }

-- | A plain logger that logs directly to stdout. Since there is no buffer,
-- having multiple threads write to this logger can cause unexpected behavior.
stderrLogger :: Logger
stderrLogger = Logger (hPutStrLn stdout) Nothing

-- | Wraps a logger to report a different logging level
levelFilteredLogger :: Maybe LogLevel -> Logger -> Logger
levelFilteredLogger f inner = inner { logLevel = f }

-- | A logger that wraps another logger and adds line buffering.
newBufferedLogger :: Logger -> IO Logger
newBufferedLogger inner = do
    channel <- newChan
    logOpen <- newMVar ()
    let writeFn = writeChan channel
    forkIO . forever $ do
        -- TODO: This thread will currently keep running until the main
        -- program exits. OK for now, but it would be cleaner to provide a
        -- cleanup function that can be used in a bracket.
        readChan channel >>= writeLogRaw inner
    return $ Logger { writeLogRaw = writeFn, logLevel = logLevel inner }
