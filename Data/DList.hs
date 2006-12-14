-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList
-- Copyright   :  (c) Don Stewart 2006
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

  DList         -- abstract, instance Monoid

  -- * Construction
  ,fromList      -- :: [a] -> DList a
  ,toList        -- :: DList a -> [a]

  -- * Basic functions
  ,empty         -- :: DList a
  ,singleton     -- :: a -> DList a
  ,cons          -- :: a -> DList a -> DList a
  ,snoc          -- :: DList a -> a -> DList a
  ,append        -- :: DList a -> DList a -> DList a
  ,concat        -- :: [DList a] -> DList a

  ) where

import Prelude hiding (concat)
import Control.Monad
import qualified Data.List as List (concat, map)
import Data.Monoid

-- | A difference list is a function that given a list, returns the
-- original contents of the difference list prepended at the given list
--
-- This structure supports /O(1)/ append and snoc operations on lists.
--
newtype DList a = DL { unDL :: [a] -> [a] }

-- | Converting a normal list to a dlist
fromList    :: [a] -> DList a
fromList    = DL . (++)

-- | Converting a dlist back to a normal list
toList      :: DList a -> [a]
toList      = ($[]) . unDL

-- | Create a difference list containing no elements
empty       :: DList a
empty       = DL id

-- | Create difference list with given single element
singleton   :: a -> DList a
singleton   = DL . (:)

-- | /O(1)/, Prepend a single element to a difference list
cons        :: a -> DList a -> DList a
cons x xs   = DL ((x:) . unDL xs)

-- | /O(1)/, Append a single element at a difference list
snoc        :: DList a -> a -> DList a
snoc xs x   = DL (unDL xs . (x:))

-- | Appending difference lists
append       :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

-- | Concatenate difference lists
concat       :: [DList a] -> DList a
concat       = foldr append empty

instance Monoid (DList a) where
    mempty  = empty
    mappend = append

instance Functor DList where
  fmap f = fromList . List.map f . toList

instance Monad DList where
  m >>= k  
    -- = concat (toList (fmap k m))
    -- = (concat . toList . fromList . List.map k . toList) m
    -- = concat . List.map k . toList $ m
    -- = foldr append empty . List.map k . toList $ m
    = foldr (append . k) empty . toList $ m
    
  return x = singleton x
  fail s   = empty

instance MonadPlus DList where
  mzero    = empty
  mplus    = append
