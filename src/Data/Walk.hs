{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}

module Data.Walk
( Walk (..)
)
where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

class Walk c e where
    walk :: (e -> e) -> (c -> c)

instance Walk JSON.Value Text where
    walk = jsonTextWalk

jsonTextWalk :: (Text -> Text) -> (JSON.Value -> JSON.Value)
jsonTextWalk f (JSON.String t) = JSON.String (f t)
jsonTextWalk f (JSON.Array v) = JSON.Array (fmap (walk f) v)
jsonTextWalk f (JSON.Object m) =
    JSON.Object . HashMap.fromList $
        [ (f k, walk f v) | (k, v) <- HashMap.toList m ]
jsonTextWalk _ x = x

instance Walk [a] a where
    walk = map

instance Walk (Maybe a) a where
    walk = fmap
