{-# OPTIONS_GHC -Wall -O2 #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList
-- Copyright   :  (c) Don Stewart 2006-2007
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  portable (Haskell 98)
--
-- Difference lists: a data structure for O(1) append on lists.
--
-----------------------------------------------------------------------------

module Data.DList (

   DList

  -- * Construction
  ,fromList      -- :: [a] -> DList a
  ,toList        -- :: DList a -> [a]
  ,apply         -- :: DList a -> [a] -> [a]

  -- * Basic functions
  ,empty         -- :: DList a
  ,singleton     -- :: a -> DList a
  ,cons          -- :: a -> DList a -> DList a
  ,snoc          -- :: DList a -> a -> DList a
  ,append        -- :: DList a -> DList a -> DList a
  ,concat        -- :: [DList a] -> DList a
  ,replicate     -- :: Int -> a -> DList a
  ,list          -- :: b -> (a -> DList a -> b) -> DList a -> b
  ,head          -- :: DList a -> a
  ,tail          -- :: DList a -> DList a
  ,unfoldr       -- :: (b -> Maybe (a, b)) -> b -> DList a
  ,foldr         -- :: (a -> b -> b) -> b -> DList a -> b
  ,map           -- :: (a -> b) -> DList a -> DList b

  -- * Monoidal interface
  , Monoid(..)
  , (<>)

  ) where

import Prelude hiding (concat, foldr, map, head, tail, replicate)
import qualified Data.List as List
import Control.Monad as M
import Data.Monoid
import Data.Function (on)

import Data.Foldable (Foldable)
import qualified Data.Foldable as F

#ifdef __GLASGOW_HASKELL__
import Text.Read (Lexeme(Ident), lexP, parens, prec, readPrec, readListPrec,
                  readListPrecDefault)
#endif

import Control.Applicative(Applicative(..), Alternative, (<|>))
import qualified Control.Applicative (empty)

-- | A difference list is a function that given a list, returns the
-- original contents of the difference list prepended at the given list
--
-- This structure supports /O(1)/ append and snoc operations on lists,
-- making it very useful for append-heavy uses, such as logging and
-- pretty printing.
--
-- For example, using DList as the state type when printing a tree with
-- the Writer monad
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
--
newtype DList a = DL { unDL :: [a] -> [a] }

-- | Converting a normal list to a dlist
fromList    :: [a] -> DList a
fromList    = DL . (++)
{-# INLINE fromList #-}

-- | Converting a dlist back to a normal list
toList      :: DList a -> [a]
toList      = ($[]) . unDL
{-# INLINE toList #-}

-- | Apply a dlist to a list to get the underlying list with an extension
--
-- > apply (fromList xs) ys = xs ++ ys
apply       :: DList a -> [a] -> [a]
apply       = unDL

-- | Create a difference list containing no elements
empty       :: DList a
empty       = DL id
{-# INLINE empty #-}

-- | Create difference list with given single element
singleton   :: a -> DList a
singleton   = DL . (:)
{-# INLINE singleton #-}

-- | /O(1)/, Prepend a single element to a difference list
infixr `cons`
cons        :: a -> DList a -> DList a
cons x xs   = DL ((x:) . unDL xs)
{-# INLINE cons #-}

-- | /O(1)/, Append a single element at a difference list
infixl `snoc`
snoc        :: DList a -> a -> DList a
snoc xs x   = DL (unDL xs . (x:))
{-# INLINE snoc #-}

-- | /O(1)/, Appending difference lists
append       :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)
{-# INLINE append #-}

-- | /O(spine)/, Concatenate difference lists
concat       :: [DList a] -> DList a
concat       = List.foldr append empty
{-# INLINE concat #-}

-- | /O(n)/, Create a difference list of the given number of elements
replicate :: Int -> a -> DList a
replicate n x = DL $ \xs -> let go m | m <= 0    = xs
                                     | otherwise = x : go (m-1)
                            in go n
{-# INLINE replicate #-}

-- | /O(length dl)/, List elimination, head, tail.
list :: b -> (a -> DList a -> b) -> DList a -> b
list nill consit dl =
  case toList dl of
    [] -> nill
    (x : xs) -> consit x (fromList xs)

-- | Return the head of the list
head :: DList a -> a
head = list (error "Data.DList.head: empty list") const

-- | Return the tail of the list
tail :: DList a -> DList a
tail = list (error "Data.DList.tail: empty list") (flip const)

-- | Unfoldr for difference lists
unfoldr :: (b -> Maybe (a, b)) -> b -> DList a
unfoldr pf b =
  case pf b of
    Nothing     -> empty
    Just (a, b') -> cons a (unfoldr pf b')

-- | Foldr over difference lists
foldr        :: (a -> b -> b) -> b -> DList a -> b
foldr f b    = List.foldr f b . toList
{-# INLINE foldr #-}

-- | Map over difference lists.
map          :: (a -> b) -> DList a -> DList b
map f        = foldr (cons . f) empty
{-# INLINE map #-}

instance Eq a => Eq (DList a) where
    (==) = (==) `on` toList

instance Ord a => Ord (DList a) where
    compare = compare `on` toList

-- The Read and Show instances were adapted from Data.Sequence.

instance Read a => Read (DList a) where
#ifdef __GLASGOW_HASKELL__
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    dl <- readPrec
    return (fromList dl)
  readListPrec = readListPrecDefault
#else
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromList", s) <- lex r
    (dl, t) <- reads s
    return (fromList dl, t)
#endif

instance Show a => Show (DList a) where
  showsPrec p dl = showParen (p > 10) $
    showString "fromList " . shows (toList dl)

instance Monoid (DList a) where
    mempty  = empty
    mappend = append

-- CPP: (<>) added in 7.4.1
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 704
-- | An infix synonym for 'mappend'
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
infixr 6 <>
#endif

instance Functor DList where
    fmap = map
    {-# INLINE fmap #-}

instance Applicative DList where
    pure  = return
    (<*>) = ap

instance Alternative DList where
    empty = empty
    (<|>) = append

instance Monad DList where
  m >>= k
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = List.foldr append empty . List.map k . toList $ m
    -- = List.foldr (append . k) empty . toList $ m
    = foldr (append . k) empty m
  {-# INLINE (>>=) #-}

  return x = singleton x
  {-# INLINE return #-}

  fail _   = empty
  {-# INLINE fail #-}

instance MonadPlus DList where
  mzero    = empty
  mplus    = append

instance Foldable DList where
  fold        = mconcat . toList
  {-# INLINE fold #-}

  foldMap f   = F.foldMap f . toList
  {-# INLINE foldMap #-}

  foldr f x   = List.foldr f x . toList
  {-# INLINE foldr #-}

  foldl f x   = List.foldl f x . toList
  {-# INLINE foldl #-}

  foldr1 f    = List.foldr1 f . toList
  {-# INLINE foldr1 #-}

  foldl1 f    = List.foldl1 f . toList
  {-# INLINE foldl1 #-}

-- CPP: foldl', foldr' added to Foldable in 7.6.1
-- http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/release-7-6-1.html
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
  foldl' f x  = List.foldl' f x . toList
  {-# INLINE foldl' #-}

  foldr' f x  = F.foldr' f x . toList
  {-# INLINE foldr' #-}
#endif

