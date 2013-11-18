{-# OPTIONS_GHC -Wall #-}

module Main (main) where

--------------------------------------------------------------------------------

import qualified Prelude   as P
import qualified Data.List as P (unfoldr)
import Prelude          hiding (head, tail, foldr, replicate)
import Text.Show.Functions ()
import Control.Arrow (second)

import Data.Foldable (foldr)
import Data.Traversable (traverse)

import Test.QuickCheck.Parallel

import Data.DList hiding (concat, map, foldr)

--------------------------------------------------------------------------------

prop_model :: [Int] -> Bool
prop_model x = (toList . fromList $ x) == id x

prop_empty :: Bool
prop_empty = ([] :: [Int]) == (toList mempty :: [Int])

prop_singleton :: Int -> Bool
prop_singleton c = [c] == toList (singleton c)

prop_cons :: Int -> [Int] -> Bool
prop_cons c xs = (c : xs) == toList (cons c (fromList xs))

prop_snoc :: [Int] -> Int -> Bool
prop_snoc xs c = (xs ++ [c]) == toList (snoc (fromList xs) c)

prop_append :: [Int] -> [Int] -> Bool
prop_append xs ys = (xs ++ ys) == toList (fromList xs <> fromList ys)

prop_concat :: [[Int]] -> Bool
prop_concat zss  = (concat zss) == toList (mconcat (map fromList zss))

prop_replicate :: Int -> Int -> Bool
prop_replicate n x = (P.replicate n x) == toList (replicate n x)

prop_head :: [Int] -> Property
prop_head xs = not (null xs) ==> (P.head xs) == head (fromList xs)

prop_tail :: [Int] -> Property
prop_tail xs = not (null xs) ==> (P.tail xs) == (toList . tail . fromList) xs

prop_unfoldr :: (Int -> Maybe (Int, Int)) -> Int -> Int -> Property
prop_unfoldr f x n = n >= 0 ==> take n (P.unfoldr f x)
                             == take n (toList $ unfoldr f x)

prop_foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_foldr f x xs = foldr f x xs == foldr f x (fromList xs)

prop_traverse :: (Int -> [Int]) -> [Int] -> Bool
prop_traverse f xs = fmap fromList (traverse f xs) == traverse f (fromList xs)

prop_map :: (Int -> Int) -> [Int] -> Bool
prop_map f xs = (map f xs) == (toList $ fmap f (fromList xs))

prop_map_fusion :: (Int -> Int) -> (a -> Int) -> [a] -> Bool
prop_map_fusion f g xs = (map f . map g $ xs)
                      == (toList $ fmap f . fmap g $ fromList xs)

prop_show_read :: [Int] -> Bool
prop_show_read x = (read . show) x == x

prop_read_show :: [Int] -> Bool
prop_read_show x = (show . f . read) s == s
  where
    s = "fromList " ++ show x
    f :: DList Int -> DList Int
    f = id

--------------------------------------------------------------------------------

props :: [(Name, Property)]
props =
  [ ("model",         property prop_model)
  , ("empty",         property prop_empty)
  , ("singleton",     property prop_singleton)
  , ("cons",          property prop_cons)
  , ("snoc",          property prop_snoc)
  , ("append",        property prop_append)
  , ("concat",        property prop_concat)
  , ("replicate",     property prop_replicate)
  , ("head",          property prop_head)
  , ("tail",          property prop_tail)
  , ("unfoldr",       property prop_unfoldr)
  , ("foldr",         property prop_foldr)
  , ("traverse",      property prop_traverse)
  , ("map",           property prop_map)
  , ("map fusion",    property (prop_map_fusion (+1) (+1)))
  , ("read . show",   property prop_show_read)
  , ("show . read",   property prop_read_show)
  ]

--------------------------------------------------------------------------------

{-
-- Sequential
main :: IO ()
main = quickCheck $ conjoin (map (uncurry label) props)
-}

-- Parallel
main :: IO ()
main = pRunAllProcessors 100 $ map (second pDet) props

