{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FunctionalDependencies #-}
module Web.Sprinkles.Prelude
( module P
, LText
, Packable (..)
, MapLike (..)
, TextLike (..)
, ListLike (..)
, readMay
, Cased (..)
)
where

import Prelude as P hiding (unwords, words, lookup, length, take, drop, break)

import Data.Text as P (Text)
import Data.Map as P (Map)
import Data.HashMap.Strict as P (HashMap)
import Data.Set as P (Set)
import Data.HashSet as P (HashSet)
import Data.Maybe as P (fromMaybe, catMaybes)
import Data.String as P (IsString (..))
import Control.Monad as P
import Control.Applicative as P
import Control.Concurrent.STM as P
import Control.Concurrent.Chan as P
import Data.Time as P (UTCTime (..), getCurrentTime)
import GHC.Generics as P (Generic)
import System.IO as P (stdin, stdout, stderr)
import Text.Printf as P (printf)

import qualified Prelude
import qualified Data.List as List
import Data.Hashable (Hashable)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text.Lazy as LText
import qualified Data.Text as Text
import qualified Data.Char as Char (toLower, toUpper)
import Text.Read (readMaybe)

readMay :: (Read a, Packable t [Char]) => t -> Maybe a
readMay = readMaybe . unpack

type LText = LText.Text

class Packable t s where
  pack :: s -> t
  unpack :: t -> s

instance Packable Text [Char] where
  pack = Text.pack
  unpack = Text.unpack

instance Packable LText [Char] where
  pack = LText.pack
  unpack = LText.unpack


class MapLike m k v where
  mapFromList :: [(k,v)] -> m k v
  mapToList :: m k v -> [(k,v)]
  lookup :: k -> m k v -> Maybe v
  insertMap :: k -> v -> m k v -> m k v
  deleteMap :: k -> m k v -> m k v

instance (Eq k, Hashable k) => MapLike HashMap k v where
  mapFromList = HashMap.fromList
  mapToList = HashMap.toList
  lookup = HashMap.lookup
  insertMap = HashMap.insert
  deleteMap = HashMap.delete

instance (Ord k) => MapLike Map k v where
  mapFromList = Map.fromList
  mapToList = Map.toList
  lookup = Map.lookup
  insertMap = Map.insert
  deleteMap = Map.delete

class ListLike l c | l -> c where
  length :: l -> Int
  take :: Int -> l -> l
  drop :: Int -> l -> l
  break :: (c -> Bool) -> l -> (l, l)
  splitElem :: c -> l -> [l]

instance ListLike [a] a where
  length = List.length
  take = List.take
  drop = List.drop
  break = List.break
  splitElem = List.splitElem

instance ListLike Text Char where
  length = Text.length
  take = Text.take
  drop = Text.drop
  break = Text.break

instance ListLike LText Char where
  length = fromIntegral . LText.length
  take = LText.take . fromIntegral
  drop = LText.drop . fromIntegral
  break = LText.break

class TextLike t where
  unwords :: [t] -> t
  words :: t -> [t]
  isPrefixOf :: t -> t -> Bool
  tshow :: forall a. Show a => a -> t

instance TextLike [Char] where
  unwords = Prelude.unwords
  words = Prelude.words
  isPrefixOf = List.isPrefixOf
  tshow = show

instance TextLike Text where
  unwords = Text.unwords
  words = Text.words
  isPrefixOf = Text.isPrefixOf
  tshow = pack . show

instance TextLike LText where
  unwords = LText.unwords
  words = LText.words
  isPrefixOf = LText.isPrefixOf
  tshow = pack . show

class Cased t where
  toUpper :: t -> t
  toLower :: t -> t

instance Cased Char where
  toUpper = Char.toUpper
  toLower = Char.toLower

instance (Functor f, Cased t) => Cased (f t) where
  toUpper = fmap toUpper
  toLower = fmap toLower

instance Cased Text where
  toUpper = Text.toUpper
  toLower = Text.toLower
