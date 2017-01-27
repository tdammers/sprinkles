{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE RankNTypes #-}

module Data.Expandable
( ExpandableM (..)
, expand
)
where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad.Identity

class ExpandableM t a where
    expandM :: forall m. Monad m => (t -> m t) -> a -> m a

expand :: ExpandableM t a => (t -> t) -> a -> a
expand f x = runIdentity $ expandM (return . f) x

instance ExpandableM t t where
    expandM f x = f x

instance ExpandableM t a => ExpandableM t (Maybe a) where
    expandM _ Nothing = return Nothing
    expandM f (Just x) = Just <$> expandM f x

instance ExpandableM t a => ExpandableM t [a] where
    expandM f xs = mapM (expandM f) xs

instance ExpandableM Text JSON.Value where
    expandM = jsonTextWalk

instance (ExpandableM t a, ExpandableM t b) => ExpandableM t (a, b) where
    expandM f (x, y) = (,) <$> expandM f x <*> expandM f y

jsonTextWalk :: Monad m => (Text -> m Text) -> (JSON.Value -> m JSON.Value)
jsonTextWalk f (JSON.String t) = JSON.String <$> f t
jsonTextWalk f (JSON.Array v) = JSON.Array <$> sequence (fmap (expandM f) v)
jsonTextWalk f (JSON.Object o) =
    JSON.Object . HashMap.fromList <$> (expandM f . HashMap.toList) o
jsonTextWalk _ x = return x

