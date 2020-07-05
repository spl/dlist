-- Options passed to GHC
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- CPP: GHC >= 7.8 for overloaded lists
#if __GLASGOW_HASKELL__ >= 708
-- For the IsList test
{-# LANGUAGE OverloadedLists #-}
#endif

-- CPP: GHC == 7.8 for using pattern synonyms
#if __GLASGOW_HASKELL__ == 708
{-# LANGUAGE PatternSynonyms #-}
#endif

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import Data.DList
import qualified Data.List as List
import OverloadedStrings (testOverloadedStrings)
import Test.QuickCheck
import Text.Show.Functions ()
import Prelude hiding (concat, foldr, head, map, replicate, tail)
import qualified Data.Traversable as Traversable

#if MIN_VERSION_base(4,9,0)
-- base-4.9 introduced Semigroup and NonEmpty.
import Control.Applicative (liftA2) -- Arbitrary1 NonEmpty instance
import Data.Maybe (mapMaybe) -- Arbitrary1 NonEmpty instance
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import Data.Semigroup (Semigroup(..))
#endif

--------------------------------------------------------------------------------

eqWith :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
eqWith f g x = f x == g x

eqOn :: Eq b => (a -> Bool) -> (a -> b) -> (a -> b) -> a -> Property
eqOn c f g x = c x ==> f x == g x

--------------------------------------------------------------------------------

prop_model :: [Int] -> Bool
prop_model = eqWith id (toList . fromList)

prop_empty :: Bool
prop_empty = ([] :: [Int]) == (toList empty :: [Int])

prop_singleton :: Int -> Bool
prop_singleton = eqWith (: []) (toList . singleton)

prop_cons :: Int -> [Int] -> Bool
prop_cons c = eqWith (c :) (toList . cons c . fromList)

prop_snoc :: [Int] -> Int -> Bool
prop_snoc xs c = xs ++ [c] == toList (snoc (fromList xs) c)

prop_append :: [Int] -> [Int] -> Bool
prop_append xs ys = xs ++ ys == toList (fromList xs `append` fromList ys)

prop_concat :: [[Int]] -> Bool
prop_concat = eqWith List.concat (toList . concat . List.map fromList)

-- The condition reduces the size of replications and thus the eval time.
prop_replicate :: Int -> Int -> Property
prop_replicate n =
  eqOn (const (n < 100)) (List.replicate n) (toList . replicate n)

prop_head :: [Int] -> Property
prop_head = eqOn (not . null) List.head (head . fromList)

prop_tail :: [Int] -> Property
prop_tail = eqOn (not . null) List.tail (toList . tail . fromList)

prop_unfoldr :: (Int -> Maybe (Int, Int)) -> Int -> Int -> Property
prop_unfoldr f n =
  eqOn (const (n >= 0)) (take n . List.unfoldr f) (take n . toList . unfoldr f)

prop_foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Bool
prop_foldr f x = eqWith (List.foldr f x) (foldr f x . fromList)

prop_map :: (Int -> Int) -> [Int] -> Bool
prop_map f = eqWith (List.map f) (toList . map f . fromList)

prop_map_fusion :: (Int -> Int) -> (a -> Int) -> [a] -> Bool
prop_map_fusion f g =
  eqWith (List.map f . List.map g) (toList . map f . map g . fromList)

prop_intercalate :: [Int] -> [[Int]] -> Bool
prop_intercalate sep =
  eqWith (List.intercalate sep) (toList . intercalate (fromList sep) . List.map fromList)

prop_show_read :: [Int] -> Bool
prop_show_read = eqWith id (read . show)

prop_read_show :: [Int] -> Bool
prop_read_show x = eqWith id (show . f . read) $ "fromList " ++ show x
  where
    f :: DList Int -> DList Int
    f = id

prop_fail :: String -> Bool
prop_fail str = fail str == (empty :: DList ())

prop_Traversable_traverse :: [Int] -> Bool
prop_Traversable_traverse xs = (==)
  (Traversable.traverse pure xs :: [[Int]])
  (fmap toList (Traversable.traverse pure (fromList xs)))

-- CPP: GHC >= 7.8 for overloaded lists
#if __GLASGOW_HASKELL__ >= 708

-- | Test that the IsList instance methods compile and work with simple lists
prop_IsList :: Bool
prop_IsList = test_fromList [1,2,3] && test_toList (fromList [1, 2, 3])
  where
    test_fromList, test_toList :: DList Int -> Bool
    test_fromList x = x == fromList [1, 2, 3]
    test_toList [1, 2, 3] = True
    test_toList _ = False

-- | Test the pattern synonyms
prop_patterns :: [Int] -> Bool
prop_patterns xs = case fromList xs of
  Nil -> xs == []
  Cons y ys -> xs == (y : ys)
  _ -> False

#endif

-- CPP: base >= 4.9 for Semigroup
#if MIN_VERSION_base(4,9,0)

prop_Semigroup_append :: [Int] -> [Int] -> Bool
prop_Semigroup_append xs ys = xs <> ys == toList (fromList xs <> fromList ys)

-- We include the instances for NonEmpty because QuickCheck (>= 2.10) does not.
-- We could alternatively depend on quickcheck-instances (>= 0.3.15), but
-- quickcheck-instances has sometimes lagged behind newer GHC/base versions. By
-- including the instances here, we do not need to track the
-- quickcheck-instances version, thus simplifying dlist.cabal and reducing the
-- maintenance effort.

instance Arbitrary1 NonEmpty where
  liftArbitrary arb = liftA2 (:|) arb (liftArbitrary arb)
  liftShrink shr (x :| xs) = mapMaybe nonEmpty . liftShrink shr $ x : xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary1
  shrink = shrink1

prop_Semigroup_sconcat :: NonEmpty [Int] -> Bool
prop_Semigroup_sconcat xs = sconcat xs == toList (sconcat (fmap fromList xs))

prop_Semigroup_stimes :: Int -> [Int] -> Bool
prop_Semigroup_stimes n xs =
  n < 0 || stimes n xs == toList (stimes n (fromList xs))

#endif

--------------------------------------------------------------------------------

props :: [(String, Property)]
props =
  [ ("model", property prop_model),
    ("empty", property prop_empty),
    ("singleton", property prop_singleton),
    ("cons", property prop_cons),
    ("snoc", property prop_snoc),
    ("append", property prop_append),
    ("concat", property prop_concat),
    ("replicate", property prop_replicate),
    ("head", property prop_head),
    ("tail", property prop_tail),
    ("fail", property prop_fail),
    ("unfoldr", property prop_unfoldr),
    ("foldr", property prop_foldr),
    ("map", property prop_map),
    ("map fusion", property (prop_map_fusion (+ 1) (+ 1))),
    ("intercalate", property prop_intercalate),
    ("read . show", property prop_show_read),
    ("show . read", property prop_read_show),
    ("Traversable traverse", property prop_Traversable_traverse)
-- CPP: GHC >= 7.8 for IsList, pattern synonyms
#if __GLASGOW_HASKELL__ >= 708
    ,
    ("IsList",               property prop_IsList),
    ("patterns",             property prop_patterns)
#endif
-- CPP: base >= 4.9 for Semigroup
#if MIN_VERSION_base(4,9,0)
    ,
    ("Semigroup <>",         property prop_Semigroup_append),
    ("Semigroup sconcat",    property prop_Semigroup_sconcat),
    ("Semigroup stimes",     property prop_Semigroup_stimes)
#endif
  ]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  testOverloadedStrings
  quickCheck $ conjoin $ List.map (uncurry label) props
