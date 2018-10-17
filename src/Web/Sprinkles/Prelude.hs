{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RankNTypes #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE OverloadedStrings #-}
module Web.Sprinkles.Prelude
( module P
, LText
, LByteString
, Packable (..)
, MapLike (..)
, SetLike (..)
, TextLike (..)
, ListLike (..)
, StrictnessConvert (..)
, Encode (..)
, splitElem
, readMay
, Cased (..)
, throwM
, FileIO (..)
, hPut
, (++)
, concat
, empty
, getArgs
, getEnv
, lookupEnv
)
where

import Prelude as P hiding ( unwords
                           , words
                           , lookup
                           , length
                           , take
                           , drop
                           , takeWhile
                           , dropWhile
                           , null
                           , tail
                           , break
                           , filter
                           , unlines
                           , putStr
                           , putStrLn
                           , hPutStr
                           , hPutStrLn
                           , readFile
                           , writeFile
                           , getContents
                           , hGetContents
                           , (++)
                           , concat
                           , empty
                           )

import Data.Text as P (Text)
import Data.ByteString as P (ByteString)
import Data.Hashable (Hashable)
import Data.Map as P (Map)
import Data.HashMap.Strict as P (HashMap)
import Data.Set as P (Set)
import Data.HashSet as P (HashSet)
import Data.Hashable as P (Hashable (..))
import Data.Maybe as P (fromMaybe, catMaybes, isNothing, isJust)
import Data.String as P (IsString (..))
import Data.Vector as P (Vector)
import Data.List as P (sortOn)
import Data.Word as P (Word8, Word16, Word32, Word64, Word)
import Data.Int as P (Int8, Int16, Int32, Int64)
import Control.Monad as P
import Data.Semigroup as P hiding (getAll, All)
import Data.Monoid as P hiding (getFirst, getLast, First, Last, getAll, All)
import Control.Monad.IO.Class as P
import Control.Exception as P (bracket, bracket_, throw, catch)
import Control.Applicative as P hiding (empty)
import Data.IORef as P
import Control.Concurrent as P
import Control.Concurrent.STM as P
import Control.Concurrent.Chan as P
import Control.Concurrent.MVar as P
import Control.Exception as P hiding (throw)
import Data.Time as P (UTCTime (..), getCurrentTime)
import GHC.Generics as P (Generic)
import System.IO as P (stdin, stdout, stderr, Handle)
import System.IO.Error as P
import System.FilePath as P
import Text.Printf as P (printf)
import Control.Monad.Identity as P

import qualified Prelude
import qualified Data.List as List
import qualified Data.List.Split as List (splitWhen, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Char as Char (toLower, toUpper)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Vector as Vector
import qualified System.IO
import qualified System.Environment as Env
import Text.Read (readMaybe)
import Control.Exception (throw)

readMay :: (Read a, Packable t [Char]) => t -> Maybe a
readMay = readMaybe . unpack

throwM :: (Exception e, Monad m) => e -> m a
throwM = throw

type LText = LText.Text

type LByteString = LBS.ByteString

(++) :: Semigroup m => m -> m -> m
(++) = (<>)

concat :: Monoid m => [m] -> m
concat = mconcat

empty :: Monoid m => m
empty = mempty

class Packable t s | t -> s where
  pack :: s -> t
  unpack :: t -> s

instance Packable Text [Char] where
  pack = Text.pack
  unpack = Text.unpack

instance Packable LText [Char] where
  pack = LText.pack
  unpack = LText.unpack

instance Packable [a] [a] where
  pack = id
  unpack = id


class MapLike m k v | m -> k, m -> v where
  mapFromList :: [(k,v)] -> m
  mapToList :: m -> [(k,v)]
  lookup :: k -> m -> Maybe v
  insertMap :: k -> v -> m -> m
  deleteMap :: k -> m -> m
  keys :: m -> [k]

instance (Eq k, Hashable k) => MapLike (HashMap k v) k v where
  mapFromList = HashMap.fromList
  mapToList = HashMap.toList
  lookup = HashMap.lookup
  insertMap = HashMap.insert
  deleteMap = HashMap.delete
  keys = HashMap.keys

instance (Ord k) => MapLike (Map k v) k v where
  mapFromList = Map.fromList
  mapToList = Map.toList
  lookup = Map.lookup
  insertMap = Map.insert
  deleteMap = Map.delete
  keys = Map.keys

instance (Ord k, Eq k) => MapLike [(k, v)] k v where
  mapFromList = id
  mapToList = id
  lookup = List.lookup
  insertMap = \k v -> ((k,v):)
  deleteMap = \k -> List.filter ((/= k) . fst)
  keys = map fst

class SetLike l c | l -> c where
  setFromList :: [c] -> l

instance Ord a => SetLike (Set a) a where
  setFromList = Set.fromList

class ListLike l c | l -> c where
  length :: l -> Int
  take :: Int -> l -> l
  drop :: Int -> l -> l
  takeWhile :: (c -> Bool) -> l -> l
  dropWhile :: (c -> Bool) -> l -> l
  break :: (c -> Bool) -> l -> (l, l)
  splitWhen :: (c -> Bool) -> l -> [l]
  splitSeq :: Eq c => l -> l -> [l]
  splitSeq = \sep lst ->
    map fromList $ List.splitOn (toList sep) (toList lst)
  toList :: l -> [c]
  fromList :: [c] -> l
  null :: l -> Bool
  headMay :: l -> Maybe c
  headMay = \xs -> case toList xs of
    (x:_) -> Just x
    _ -> Nothing
  tail :: l -> l
  tail = drop 1
  intercalate :: l -> [l] -> l
  filter :: (c -> Bool) -> l -> l
  filter = \p -> fromList . List.filter p . toList

splitElem :: (Eq c, ListLike l c) => c -> l -> [l]
splitElem c = splitWhen (== c)

instance ListLike [a] a where
  length = List.length
  take = List.take
  drop = List.drop
  takeWhile = List.takeWhile
  dropWhile = List.dropWhile
  break = List.break
  splitWhen = List.splitWhen
  toList = id
  fromList = id
  null = List.null
  intercalate = List.intercalate
  filter = List.filter

instance ListLike (Vector a) a where
  length = Vector.length
  take = Vector.take
  drop = Vector.drop
  takeWhile = Vector.takeWhile
  dropWhile = Vector.dropWhile
  break = Vector.break
  splitWhen = splitVectorWhen
  toList = Vector.toList
  fromList = Vector.fromList
  null = Vector.null
  intercalate = mintercalate
  filter = Vector.filter

mintercalate :: Monoid m => m -> [m] -> m
mintercalate _ [] = mempty
mintercalate _ [x] = x
mintercalate sep (x:xs) = x <> sep <> mintercalate sep xs

splitVectorWhen :: (a -> Bool) -> Vector a -> [Vector a]
splitVectorWhen p v = case Vector.findIndex p v of
  Nothing ->
    [v]
  Just index ->
    let (current, remainder) = Vector.splitAt index v
    in current : splitVectorWhen p remainder

instance ListLike Text Char where
  length = Text.length
  take = Text.take
  drop = Text.drop
  takeWhile = Text.takeWhile
  dropWhile = Text.dropWhile
  break = Text.break
  splitWhen = Text.split
  toList = Text.unpack
  fromList = Text.pack
  null = Text.null
  intercalate = Text.intercalate
  filter = Text.filter

instance ListLike LText Char where
  length = fromIntegral . LText.length
  take = LText.take . fromIntegral
  drop = LText.drop . fromIntegral
  takeWhile = LText.takeWhile
  dropWhile = LText.dropWhile
  break = LText.break
  splitWhen = LText.split
  toList = LText.unpack
  fromList = LText.pack
  null = LText.null
  intercalate = LText.intercalate
  filter = LText.filter

instance ListLike ByteString Word8 where
  length = BS.length
  take = BS.take
  drop = BS.drop
  takeWhile = BS.takeWhile
  dropWhile = BS.dropWhile
  break = BS.break
  splitWhen = BS.splitWith
  toList = BS.unpack
  fromList = BS.pack
  null = BS.null
  intercalate = BS.intercalate
  filter = BS.filter

instance ListLike LByteString Word8 where
  length = fromIntegral . LBS.length
  take = LBS.take . fromIntegral
  drop = LBS.drop . fromIntegral
  takeWhile = LBS.takeWhile
  dropWhile = LBS.dropWhile
  break = LBS.break
  splitWhen = LBS.splitWith
  toList = LBS.unpack
  fromList = LBS.pack
  null = LBS.null
  intercalate = LBS.intercalate
  filter = LBS.filter

class (Monoid t, Semigroup t, IsString t) => TextLike t where
  unwords :: [t] -> t
  words :: t -> [t]
  isPrefixOf :: t -> t -> Bool
  isSuffixOf :: t -> t -> Bool
  tshow :: forall a. Show a => a -> t
  unlines :: [t] -> t
  unlines = mintercalate "\n"

instance TextLike [Char] where
  unwords = Prelude.unwords
  words = Prelude.words
  isPrefixOf = List.isPrefixOf
  isSuffixOf = List.isSuffixOf
  tshow = show

instance TextLike Text where
  unwords = Text.unwords
  words = Text.words
  isPrefixOf = Text.isPrefixOf
  isSuffixOf = Text.isSuffixOf
  tshow = pack . show
  unlines = Text.unlines

instance TextLike LText where
  unwords = LText.unwords
  words = LText.words
  isPrefixOf = LText.isPrefixOf
  isSuffixOf = LText.isSuffixOf
  tshow = pack . show
  unlines = LText.unlines

class Encode decoded encoded | decoded -> encoded, encoded -> decoded where
  encodeUtf8 :: decoded -> encoded
  decodeUtf8 :: encoded -> decoded

instance Encode Text ByteString where
  encodeUtf8 = Text.encodeUtf8
  decodeUtf8 = Text.decodeUtf8

instance Encode LText LByteString where
  encodeUtf8 = LText.encodeUtf8
  decodeUtf8 = LText.decodeUtf8

class StrictnessConvert strict lazy | strict -> lazy, lazy -> strict where
  toStrict :: lazy -> strict
  fromStrict :: strict -> lazy

instance StrictnessConvert ByteString LByteString where
  toStrict = LBS.toStrict
  fromStrict = LBS.fromStrict

instance StrictnessConvert Text LText where
  toStrict = LText.toStrict
  fromStrict = LText.fromStrict

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

class (Semigroup s, IsString s) => FileIO s where
  getContents :: IO s
  readFile :: FilePath -> IO s
  writeFile :: FilePath -> s -> IO ()
  hGetContents :: Handle -> IO s
  putStr :: s -> IO ()
  putStrLn :: s -> IO ()
  putStrLn = putStr . (<> "\n")
  hPutStr :: Handle -> s -> IO ()
  hPutStrLn :: Handle -> s -> IO ()
  hPutStrLn = \h -> hPutStr h . (<> "\n")

instance FileIO String where
  getContents = System.IO.getContents 
  readFile = System.IO.readFile 
  writeFile = System.IO.writeFile 
  hGetContents = System.IO.hGetContents 
  putStr = System.IO.putStr 
  putStrLn = System.IO.putStrLn 
  hPutStr = System.IO.hPutStr 
  hPutStrLn = System.IO.hPutStrLn 

instance FileIO Text where
  getContents = Text.getContents 
  readFile = Text.readFile 
  writeFile = Text.writeFile 
  hGetContents = Text.hGetContents 
  putStr = Text.putStr 
  putStrLn = Text.putStrLn 
  hPutStr = Text.hPutStr 
  hPutStrLn = Text.hPutStrLn 

instance FileIO LText where
  getContents = LText.getContents 
  readFile = LText.readFile 
  writeFile = LText.writeFile 
  hGetContents = LText.hGetContents 
  putStr = LText.putStr 
  putStrLn = LText.putStrLn 
  hPutStr = LText.hPutStr 
  hPutStrLn = LText.hPutStrLn 

instance FileIO ByteString where
  getContents = BS.getContents 
  readFile = BS.readFile 
  writeFile = BS.writeFile 
  hGetContents = BS.hGetContents 
  putStr = BS.putStr 
  hPutStr = BS.hPutStr 

instance FileIO LByteString where
  getContents = LBS.getContents 
  readFile = LBS.readFile 
  writeFile = LBS.writeFile 
  hGetContents = LBS.hGetContents 
  putStr = LBS.putStr 
  hPutStr = LBS.hPutStr 

hPut :: FileIO s => Handle -> s -> IO ()
hPut = hPutStr

getArgs :: (Packable t String) => IO [t]
getArgs = map pack <$> Env.getArgs

getEnv :: (Packable v String) => String -> IO v
getEnv = fmap pack . Env.getEnv

lookupEnv :: (Packable v String) => String -> IO (Maybe v)
lookupEnv = fmap (fmap pack) . Env.lookupEnv
