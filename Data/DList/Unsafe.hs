-- Options passed to GHC
{-# OPTIONS_GHC -O2 #-}
-- Options passed to Haddock
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- For the IsList and IsString instances
{-# LANGUAGE TypeFamilies #-}

-- CPP: GHC >= 7.8 for pattern synonyms, Safe Haskell, view patterns
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}

-- The 'Data.DList.Unsafe' module exports 'UnsafeDList' and 'unsafeApplyDList',
-- which allow breaking the invariant of the 'DList' newtype. Therefore, we
-- explicitly mark 'Data.DList.Unsafe' as unsafe.
{-# LANGUAGE Unsafe #-}

{-# LANGUAGE ViewPatterns #-}
#endif

-----------------------------------------------------------------------------

-- |
-- Module: Data.DList.Unsafe
-- Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
-- License: BSD-3-Clause
--
-- Maintainer: sean.leather@gmail.com
-- Stability: stable
-- Portability: portable
--
-- This module exports everything related to 'DList', including the constructor,
-- 'UnsafeDList', and the record label, 'unsafeApplyDList', both of which can be
-- used to create unsafe 'DList' values that break the invariant preserved by
-- the names exported from 'Data.DList'.
module Data.DList.Unsafe where

-----------------------------------------------------------------------------

-- CPP: base >= 4.9 for Semigroup and MonadFail
#if MIN_VERSION_base(4,9,0)
import Data.Semigroup (Semigroup(..), stimesMonoid)

-- CPP: base < 4.13 for MonadFail not exported from Control.Monad
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail(..))
#endif

#endif

-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
import qualified GHC.Exts
#endif

import qualified Control.Applicative as Applicative
import Control.DeepSeq (NFData (..))
import Control.Monad as Monad
import qualified Data.Monoid as Monoid
import qualified Data.Foldable as Foldable
import Data.Function (on)
import qualified Data.List as List
import Data.String (IsString (..))
import qualified Data.Traversable as Traversable
import Prelude hiding (concat, foldr, head, map, replicate, tail)
import qualified Text.Read as Read

-----------------------------------------------------------------------------

-- | A difference list is a function that, given a list, returns the original
-- contents of the difference list prepended to the given list.
--
-- This structure supports /O(1)/ append and snoc operations on lists, making it
-- very useful for append-heavy uses (esp. left-nested uses of 'List.++'), such
-- as logging and pretty printing.
--
-- Here is an example using DList as the state type when printing a tree with
-- the Writer monad:
--
-- > import Control.Monad.Writer
-- > import Data.DList
-- >
-- > data Tree a = Leaf a | Branch (Tree a) (Tree a)
-- >
-- > flatten_writer :: Tree x -> DList x
-- > flatten_writer = snd . runWriter . flatten
-- >     where
-- >       flatten (Leaf x)     = tell (singleton x)
-- >       flatten (Branch x y) = flatten x >> flatten y
newtype DList a = UnsafeDList {unsafeApplyDList :: [a] -> [a]}

-- | Convert a list to a dlist
{-# INLINE fromList #-}
fromList :: [a] -> DList a
fromList = UnsafeDList . (++)

-- | Convert a dlist to a list
{-# INLINE toList #-}
toList :: DList a -> [a]
toList = ($ []) . unsafeApplyDList

-- CPP: GHC >= 7.8 for pattern synonyms
#if __GLASGOW_HASKELL__ >= 708

-- CPP: GHC >= 7.10 for pattern synonym signatures

-- | A unidirectional pattern synonym using 'toList' in a view pattern and
-- matching on @[]@
#if __GLASGOW_HASKELL__ >= 710
pattern Nil :: DList a
#endif
pattern Nil <- (toList -> [])

-- | A unidirectional pattern synonym using 'toList' in a view pattern and
-- matching on @x:xs@ such that you have the pattern @Cons x xs@
#if __GLASGOW_HASKELL__ >= 710
pattern Cons :: a -> [a] -> DList a
#endif
pattern Cons x xs <- (toList -> x : xs)

#endif

-- | Apply a dlist to a list to get the underlying list with an extension
--
-- > apply (fromList xs) ys = xs ++ ys
{-# INLINE apply #-}
apply :: DList a -> [a] -> [a]
apply = unsafeApplyDList

-- | Create a dlist containing no elements
{-# INLINE empty #-}
empty :: DList a
empty = UnsafeDList id

-- | Create dlist with a single element
{-# INLINE singleton #-}
singleton :: a -> DList a
singleton = UnsafeDList . (:)

infixr 9 `cons`

-- | /O(1)/. Prepend a single element to a dlist
{-# INLINE cons #-}
cons :: a -> DList a -> DList a
cons x xs = UnsafeDList ((x :) . unsafeApplyDList xs)

infixl 9 `snoc`

-- | /O(1)/. Append a single element to a dlist
{-# INLINE snoc #-}
snoc :: DList a -> a -> DList a
snoc xs x = UnsafeDList (unsafeApplyDList xs . (x :))

-- | /O(1)/. Append dlists
{-# INLINE append #-}
append :: DList a -> DList a -> DList a
append xs ys = UnsafeDList (unsafeApplyDList xs . unsafeApplyDList ys)

-- | /O(spine)/. Concatenate dlists
{-# INLINE concat #-}
concat :: [DList a] -> DList a
concat = List.foldr append empty

-- | /O(n)/. Create a dlist of the given number of elements
{-# INLINE replicate #-}
replicate :: Int -> a -> DList a
replicate n x = UnsafeDList $ \xs ->
  let go m
        | m <= 0 = xs
        | otherwise = x : go (m -1)
   in go n

-- | /O(n)/. List elimination for dlists
list :: b -> (a -> DList a -> b) -> DList a -> b
list nill consit dl =
  case toList dl of
    [] -> nill
    (x : xs) -> consit x (fromList xs)

-- | /O(1)/. Return the head of the dlist
{-# INLINE head #-}
head :: DList a -> a
head = list (error "Data.DList.head: empty dlist") const

-- | /O(n)/. Return the tail of the dlist
{-# INLINE tail #-}
tail :: DList a -> DList a
tail = list (error "Data.DList.tail: empty dlist") (flip const)

-- | /O(n)/. Unfoldr for dlists
unfoldr :: (b -> Maybe (a, b)) -> b -> DList a
unfoldr pf b =
  case pf b of
    Nothing -> empty
    Just (a, b') -> cons a (unfoldr pf b')

-- | /O(n)/. Foldr over difference lists
{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> DList a -> b
foldr f b = List.foldr f b . toList

-- | /O(n)/. Map over difference lists.
{-# INLINE map #-}
map :: (a -> b) -> DList a -> DList b
map f = foldr (cons . f) empty

-- | /O(spine)/. Intercalate over difference lists
{-# INLINE intercalate #-}
intercalate :: DList a -> [DList a] -> DList a
intercalate sep = concat . List.intersperse sep

instance Eq a => Eq (DList a) where
  (==) = (==) `on` toList

instance Ord a => Ord (DList a) where
  compare = compare `on` toList

-- The 'Read' and 'Show' instances were adapted from 'Data.Sequence'.

instance Read a => Read (DList a) where
  readPrec = Read.parens $ Read.prec 10 $ do
    Read.Ident "fromList" <- Read.lexP
    dl <- Read.readPrec
    return (fromList dl)
  readListPrec = Read.readListPrecDefault

instance Show a => Show (DList a) where
  showsPrec p dl =
    showParen (p > 10) $
      showString "fromList " . shows (toList dl)

instance Monoid.Monoid (DList a) where
  {-# INLINE mempty #-}
  mempty = empty

  {-# INLINE mappend #-}
  mappend = append

instance Functor DList where
  {-# INLINE fmap #-}
  fmap = map

instance Applicative.Applicative DList where
  {-# INLINE pure #-}
  pure = singleton

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Applicative.Alternative DList where
  {-# INLINE empty #-}
  empty = empty

  {-# INLINE (<|>) #-}
  (<|>) = append

instance Monad DList where
  {-# INLINE (>>=) #-}
  m >>= k =
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
    foldr (append . k) empty m

  {-# INLINE return #-}
  return = Applicative.pure

-- CPP: base < 4.13 for fail in Monad
#if !MIN_VERSION_base(4,13,0)
  {-# INLINE fail #-}
  fail _ = empty
#endif

-- CPP: base >= 4.9 for MonadFail
#if MIN_VERSION_base(4,9,0)
instance MonadFail DList where
  {-# INLINE fail #-}
  fail _ = empty
#endif

instance MonadPlus DList where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = append

instance Foldable.Foldable DList where
  {-# INLINE fold #-}
  fold = Monoid.mconcat . toList

  {-# INLINE foldMap #-}
  foldMap f = Foldable.foldMap f . toList

  {-# INLINE foldr #-}
  foldr f x = List.foldr f x . toList

  {-# INLINE foldl #-}
  foldl f x = List.foldl f x . toList

  {-# INLINE foldr1 #-}
  foldr1 f = List.foldr1 f . toList

  {-# INLINE foldl1 #-}
  foldl1 f = List.foldl1 f . toList

-- CPP: GHC >= 7.6 for foldl', foldr' in Foldable
#if __GLASGOW_HASKELL__ >= 706
  {-# INLINE foldl' #-}
  foldl' f x = List.foldl' f x . toList

  {-# INLINE foldr' #-}
  foldr' f x = Foldable.foldr' f x . toList
#endif

-- CPP: base >= 4.8 for toList in Foldable
#if MIN_VERSION_base(4,8,0)
  {-# INLINE toList #-}
  toList = Data.DList.Unsafe.toList
#endif

instance Traversable.Traversable DList where
  {-# INLINE traverse #-}
  traverse f = foldr cons_f (Applicative.pure empty)
    where
      cons_f x = Applicative.liftA2 cons (f x)

instance NFData a => NFData (DList a) where
  {-# INLINE rnf #-}
  rnf = rnf . toList

-- This is _not_ a flexible instance to allow certain uses of overloaded
-- strings. See tests/OverloadedStrings.hs for an example and
-- https://gitlab.haskell.org/ghc/ghc/-/commit/b225b234a6b11e42fef433dcd5d2a38bb4b466bf
-- for the same change made to the IsString instance for lists.
instance a ~ Char => IsString (DList a) where
  {-# INLINE fromString #-}
  fromString = fromList

-- CPP: GHC >= 7.8 for IsList
#if __GLASGOW_HASKELL__ >= 708
instance GHC.Exts.IsList (DList a) where
  type Item (DList a) = a

  {-# INLINE fromList #-}
  fromList = fromList

  {-# INLINE toList #-}
  toList = toList
#endif

-- CPP: base >= 4.9 for Semigroup
#if MIN_VERSION_base(4,9,0)
instance Semigroup (DList a) where
  {-# INLINE (<>) #-}
  (<>) = append

  -- We use 'compare n 0' since the same expression is used in 'stimesMonoid'
  -- and we should get a lazy advantage. However, we prefer the error to be
  -- sourced here instead of 'stimesMonoid'.
  stimes n = case compare n 0 of
    LT -> error "Data.DList.stimes: negative multiplier"
    _ -> stimesMonoid n
#endif
