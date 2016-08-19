{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE DeriveGeneric #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Templar.Logger
( stderrLogger
, newBufferedLogger
, syslogLogger
, nullLogger
, Logger (..)
, LogLevel (..)
, writeLog
)
where

import ClassyPrelude
import Control.Concurrent (forkIO)
import Data.Aeson (FromJSON (..), Value (..), (.:))
import qualified System.Posix.Syslog as Syslog

data LogLevel = Debug
              | Notice
              | Warning
              | Error
              | Critical
              deriving (Show, Eq, Ord, Enum, Bounded, Generic)

logLevelToSyslogPrio :: LogLevel -> Syslog.Priority
logLevelToSyslogPrio Debug = Syslog.Debug
logLevelToSyslogPrio Notice = Syslog.Notice
logLevelToSyslogPrio Warning = Syslog.Warning
logLevelToSyslogPrio Error = Syslog.Error
logLevelToSyslogPrio Critical = Syslog.Critical

instance FromJSON LogLevel where
    parseJSON (String "debug") = return Debug
    parseJSON (String "notice") = return Notice
    parseJSON (String "warning") = return Warning
    parseJSON (String "error") = return Error
    parseJSON (String "critical") = return Critical
    parseJSON x = fail $ "Invalid log level: " <> show x

data LogMessage =
    LogMessage
        { lmTimestamp :: !UTCTime
        , lmLevel :: !LogLevel
        , lmMessage :: !Text
        }
        deriving (Show, Eq)

instance FromJSON LogMessage where
    parseJSON (Object o) =
        LogMessage <$> o .: "timestamp"
                   <*> o .: "level"
                   <*> o .: "message"

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
        { writeLogMessage :: LogMessage -> IO ()
        }

nullLogger :: Logger
nullLogger = Logger . const . return $ ()

-- | A plain logger that logs directly to stdout. Since there is no buffer,
-- having multiple threads write to this logger can cause unexpected behavior.
stderrLogger :: LogLevel -> Logger
stderrLogger level =
    Logger go
    where
        go msg =
            when
                (lmLevel msg >= level)
                (hPutStrLn stderr $ formatMessage msg)

-- | A plain logger that logs to syslog.
syslogLogger :: LogLevel -> Logger
syslogLogger level = Logger go
    where
        go msg =
            Syslog.withSyslog
                "templar"
                []
                Syslog.USER
                (Syslog.logUpTo $ logLevelToSyslogPrio level) $ do
                    Syslog.syslog
                        (logLevelToSyslogPrio . lmLevel $ msg)
                        (unpack . lmMessage $ msg)

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
        readChan channel >>= writeLogMessage inner
    return $ Logger writeFn
