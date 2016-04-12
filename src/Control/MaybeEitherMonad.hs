module Control.MaybeEitherMonad
where

import Control.Exception

maybeFail :: Monad m => Maybe a -> m a
maybeFail = maybe (fail "Nothing") return

eitherFailS :: Monad m => Either String a -> m a
eitherFailS = either fail return

eitherFail :: (Show s, Monad m) => Either s a -> m a
eitherFail = either (fail . show) return

data NothingException = NothingException
    deriving (Show)

instance Exception NothingException where

data LeftException = LeftException String
    deriving (Show)

instance Exception LeftException where

maybeThrow :: Maybe a -> a
maybeThrow = maybe (throw NothingException) id

eitherThrowS :: Either String a -> a
eitherThrowS = either (throw . LeftException) id

eitherThrow :: Show err => Either err a -> a
eitherThrow = either (throw . LeftException . show) id
