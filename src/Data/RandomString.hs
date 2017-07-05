{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE ScopedTypeVariables #-}

module Data.RandomString
( randomStr
)
where

import ClassyPrelude
import System.Random

randomStr :: [Char] -> Int -> IO String
randomStr alphabet desiredLength = do
    let alphabetSize :: Int
        alphabetSize = ClassyPrelude.length alphabet
    when (alphabetSize < 1) $
        error "randomStr: empty list of allowed characters"
    items :: [String] <- forM [0..desiredLength] $ \_ -> do
        i :: Int <- randomRIO (0, pred alphabetSize)
        return $ take 1 . drop i $ alphabet
    return $ concat items
