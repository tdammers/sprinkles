{-#LANGUAGE NoImplicitPrelude #-}
module Web.Templar.Logger
( stderrLogger
, newBufferedLogger
, Logger
, writeLog
)
where

import ClassyPrelude
import Control.Concurrent (forkIO)

data Logger =
    Logger
        { writeLog :: Text -> IO ()
        }

-- | A plain logger that logs directly to stdout. Since there is no buffer,
-- having multiple threads write to this logger can cause unexpected behavior.
stderrLogger :: Logger
stderrLogger = Logger (hPutStrLn stdout)

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
        readChan channel >>= writeLog inner
    return $ Logger { writeLog = writeFn }
