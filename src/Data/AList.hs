{-#LANGUAGE NoImplicitPrelude #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE DeriveFunctor #-}
module Data.AList
( AList
, toList
, fromList
, empty
, singleton
)
where

import Web.Sprinkles.Prelude hiding (fromList, toList, singleton, empty)
import Data.Aeson (ToJSON (..), FromJSON (..), Value (..))
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid
import Data.Text (Text)
import qualified Data.Vector as Vector
import Control.Monad (forM)

newtype AList k v = AList { toList :: [(k, v)] }
    deriving (Functor, Monoid, Show, Read, Eq, Semigroup)

fromList :: [(k, v)] -> AList k v
fromList = AList

instance FromJSON a => FromJSON (AList Text a) where
    parseJSON (Array items) =
        mconcat <$> forM (Vector.toList items) parseJSON
    parseJSON v@(Object obj) =
        AList . HashMap.toList <$> parseJSON v
    parseJSON x = fail "Invalid JSON value for AList"

instance ToJSON a => ToJSON (AList Text a) where
    toJSON (AList items) =
        toJSON [ HashMap.singleton k v | (k, v) <- items ]

empty :: AList k v
empty = AList []

singleton :: k -> v -> AList k v
singleton k v = AList [(k, v)]
