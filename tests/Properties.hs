{-# OPTIONS_GHC -Wall #-}

module Properties (run) where

--------------------------------------------------------------------------------

import qualified Prelude   as P
import qualified Data.List as P (unfoldr)
import Prelude          hiding (concat,map,head,tail,foldr,map,replicate)
import Text.Show.Functions ()

import Data.Foldable (foldr)
import Data.Traversable (traverse)

import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Data.DList hiding (foldr)

--------------------------------------------------------------------------------

prop_model :: [Int] -> Bool
prop_model x = (toList . fromList $ x) == id x

prop_empty :: Bool
prop_empty = ([] :: [Int]) == (toList empty :: [Int])

prop_singleton :: Int -> Bool
prop_singleton c = [c] == toList (singleton c)

prop_cons :: Int -> [Int] -> Bool
prop_cons c xs = (c : xs) == toList (cons c (fromList xs))

prop_snoc :: [Int] -> Int -> Bool
prop_snoc xs c = (xs ++ [c]) == toList (snoc (fromList xs) c)

prop_append :: [Int] -> [Int] -> Bool
prop_append xs ys = (xs ++ ys) == toList (append (fromList xs) (fromList ys))

prop_concat :: [[Int]] -> Bool
prop_concat zss  = (P.concat zss) == toList (concat (P.map fromList zss))

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
prop_map f xs = (P.map f xs) == (toList $ map f (fromList xs))

prop_map_fusion :: (Int -> Int) -> (a -> Int) -> [a] -> Bool
prop_map_fusion f g xs = (P.map f . P.map g $ xs)
                      == (toList $ map f . map g $ fromList xs)

prop_show_read :: [Int] -> Bool
prop_show_read x = (read . show) x == x

prop_read_show :: [Int] -> Bool
prop_read_show x = (show . f . read) s == s
  where
    s = "fromList " ++ show x
    f :: DList Int -> DList Int
    f = id

--------------------------------------------------------------------------------

props :: [Property]
props =
  [ label "model"       prop_model
  , label "empty"       prop_empty
  , label "singleton"   prop_singleton
  , label "cons"        prop_cons
  , label "snoc"        prop_snoc
  , label "append"      prop_append
  , label "concat"      prop_concat
  , label "replicate"   prop_replicate
  , label "head"        prop_head
  , label "tail"        prop_tail
  , label "unfoldr"     prop_unfoldr
  , label "foldr"       prop_foldr
  , label "traverse"    prop_traverse
  , label "map"         prop_map
  , label "map fusion"  (prop_map_fusion (+1) (+1))
  , label "read . show" prop_show_read
  , label "show . read" prop_read_show
  ]

run :: IO Bool
run = fmap isSuccess $ quickCheckResult (conjoin props)

