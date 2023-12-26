{-# LANGUAGE CPP #-}

#if MIN_VERSION_base(4,9,0)
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe             #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans  #-}
--------------------------------------------------------------------------------

-- | QuickCheck property tests for DNonEmpty.
module DNonEmptyProperties (properties) where

--------------------------------------------------------------------------------

import qualified Control.Applicative as Applicative
import qualified Data.DList as DList
import Data.DList.DNonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Semigroup as Semigroup
import QuickCheckUtil
import Test.QuickCheck
import Text.Show.Functions ()
import Prelude hiding (head, map, tail)
import Data.Monoid (Sum)

-- NonEmpty.append was only added in base 4.16
nonEmptyAppend :: NonEmpty a -> NonEmpty a -> NonEmpty a
nonEmptyAppend (x NonEmpty.:| xs) ys = x NonEmpty.:| (xs ++ NonEmpty.toList ys)

--------------------------------------------------------------------------------

prop_model :: DNonEmpty Int -> Bool
prop_model = eqWith id id

prop_singleton :: Int -> Bool
prop_singleton = eqWith Applicative.pure (toNonEmpty . singleton)

prop_cons :: Int -> NonEmpty Int -> Bool
prop_cons c = eqWith (NonEmpty.cons c) (toNonEmpty . cons c . fromNonEmpty)

prop_snoc :: NonEmpty Int -> Int -> Bool
prop_snoc xs c =
  xs `nonEmptyAppend` Applicative.pure c == toNonEmpty (snoc (fromNonEmpty xs) c)

prop_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_append xs ys =
  xs `nonEmptyAppend` ys == toNonEmpty (fromNonEmpty xs `append` fromNonEmpty ys)

prop_head :: NonEmpty Int -> Bool
prop_head = eqWith NonEmpty.head (head . fromNonEmpty)

prop_tail :: NonEmpty Int -> Bool
prop_tail = eqWith NonEmpty.tail (DList.toList . tail . fromNonEmpty)

prop_foldr :: Eq b => (a -> b -> b) -> b -> NonEmpty a -> Bool
prop_foldr f initial l = foldr f initial l == foldr f initial (fromNonEmpty l)

prop_foldr1 :: Eq a => (a -> a -> a) -> NonEmpty a -> Bool
prop_foldr1 f l = foldr1 f l == foldr1 f (fromNonEmpty l)

prop_foldl :: Eq b => (b -> a -> b) -> b -> NonEmpty a -> Bool
prop_foldl f initial l = foldl f initial l == foldl f initial (fromNonEmpty l)

prop_foldMap :: (Eq b, Monoid b) => (a -> b) -> NonEmpty a -> Bool
prop_foldMap f l = foldMap f l == foldMap f (fromNonEmpty l)

prop_unfoldr :: (Int -> (Int, Maybe Int)) -> Int -> Int -> Property
prop_unfoldr f n =
  eqOn
    (const (n >= 0))
    (NonEmpty.take n . NonEmpty.unfoldr f)
    (NonEmpty.take n . toNonEmpty . unfoldr f)

prop_map :: (Int -> Int) -> NonEmpty Int -> Bool
prop_map f = eqWith (NonEmpty.map f) (toNonEmpty . map f . fromNonEmpty)

prop_show_read :: NonEmpty Int -> Bool
prop_show_read = eqWith id (read . show) . fromNonEmpty

prop_inner_show_read ::
  ( Eq (f (DNonEmpty a))
  , Show (f (DNonEmpty a))
  , Read (f (DNonEmpty a))
  , Functor f
  ) => f (NonEmpty a) -> Bool
prop_inner_show_read = eqWith id (read . show) . fmap fromNonEmpty

prop_read_show :: NonEmpty Int -> Bool
prop_read_show x = eqWith id (show . f . read) $ "fromNonEmpty (" ++ show x ++ ")"
  where
    f :: DNonEmpty Int -> DNonEmpty Int
    f = id

exampleList :: [Int]
exampleList = [1, 2, 3]

exampleDNonEmpty :: DNonEmpty Int
exampleDNonEmpty = 1 :| DList.fromList [2, 3]

prop_toList :: Bool
prop_toList = toList exampleDNonEmpty == exampleList

prop_fromList :: Bool
prop_fromList = exampleDNonEmpty == fromList exampleList

prop_Semigroup_append :: NonEmpty Int -> NonEmpty Int -> Bool
prop_Semigroup_append xs ys =
  (==)
    (xs Semigroup.<> ys)
    (toNonEmpty (fromNonEmpty xs Semigroup.<> fromNonEmpty ys))

--------------------------------------------------------------------------------

newtype Single a = Single a
  deriving (Eq, Read, Show, Functor)

instance Arbitrary a => Arbitrary (Single a) where
  arbitrary = Single <$> arbitrary

instance Arbitrary a => Arbitrary (DList.DList a) where
  arbitrary = DList.fromList <$> arbitrary

instance Arbitrary a => Arbitrary (DNonEmpty a) where
  arbitrary = do
    x <- arbitrary
    xs <- arbitrary
    pure $ x :| xs

properties :: [(String, Property)]
properties =
  [ ("model", property prop_model),
    ("singleton", property prop_singleton),
    ("cons", property prop_cons),
    ("snoc", property prop_snoc),
    ("append", property prop_append),
    ("head", property prop_head),
    ("tail", property prop_tail),
    ("unfoldr", property prop_unfoldr),
    ("foldr", property (prop_foldr @Int @Int)),
    ("foldr1", property (prop_foldr1 @Int)),
    ("foldl", property (prop_foldl @Int @Int)),
    ("foldMap", property (prop_foldMap @(Sum Int) @Int)),
    ("map", property prop_map),
    ("read . show", property prop_show_read),
    ("read . show", property (prop_inner_show_read @Single @Int)),
    ("read . show", property (prop_inner_show_read @((,) Int) @(Int, Int))),
    ("read . show", property (prop_inner_show_read @Single @(DNonEmpty Int))),
    ("show . read", property prop_read_show),
    ("toList", property prop_toList),
    ("fromList", property prop_fromList),
    ("Semigroup <>", property prop_Semigroup_append)
  ]

#else

#warning Skipping DNonEmptyProperties tests due to old version of base

module DNonEmptyProperties (properties) where

import Test.QuickCheck

properties :: [(String, Property)]
properties = []

#endif
