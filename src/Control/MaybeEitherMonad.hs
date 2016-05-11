-- | A collection of utility functions to mediate between 'Maybe', 'Either',
-- other 'Monad's, and exceptions.
module Control.MaybeEitherMonad
where

import Control.Exception

-- | Return the 'Just' value, or fail on 'Nothing'
maybeFail :: Monad m => Maybe a -> m a
maybeFail = maybe (fail "Nothing") return

-- | Return the 'Right' value, or fail with the 'Left' error message.
eitherFailS :: Monad m => Either String a -> m a
eitherFailS = either fail return

-- | Return the 'Right' value, or fail with the 'Left' error value.
eitherFail :: (Show s, Monad m) => Either s a -> m a
eitherFail = either (fail . show) return

-- | Thrown when 'maybeFail' runs into a 'Nothing'
data NothingException = NothingException
    deriving (Show)

instance Exception NothingException where

-- | Thrown when 'eitherFail' or 'eitherFailS' runs into a 'Left'
data LeftException = LeftException String
    deriving (Show)

instance Exception LeftException where

-- | Get 'Just' the value, or throw a 'NothingException'
maybeThrow :: Maybe a -> a
maybeThrow = maybe (throw NothingException) id

-- | Get the 'Right' value, or throw a 'LeftException'
eitherThrowS :: Either String a -> a
eitherThrowS = either (throw . LeftException) id

-- | Get the 'Right' value, or throw a 'LeftException'
eitherThrow :: Show err => Either err a -> a
eitherThrow = either (throw . LeftException . show) id

-- | @optionally f v@ executes the @f@ action on 'Just' the value of @v@, or
-- does nothing if @v@ is 'Nothing'.
optionally :: Monad m => (a -> m ()) -> Maybe a -> m ()
optionally = maybe (return ())
