{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 && <= 8 for 'pattern' required in the export list
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
{-# LANGUAGE PatternSynonyms #-}
#endif

-- CPP: GHC >= 7.8 for Safe Haskell
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-

The 'Data.DList' module imports the unsafe module 'Data.DList.Unsafe' but
exports only its safe aspects. Specifically, it does not export the 'DList'
constructor 'UnsafeDList' or record label 'unsafeApplyDList'. Therefore, we mark
'Data.DList' as trustworthy.

-}
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList
-- Copyright   :  (c) 2006-2009 Don Stewart, 2013-2020 Sean Leather
-- License     :  See license.md file
--
-- Maintainer  :  sean.leather@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Difference lists: a data structure for /O(1)/ append on lists.
--
-----------------------------------------------------------------------------

module Data.DList

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
  ( DList(Nil, Cons)
#else
  ( DList
#endif

  -- * Construction
  , fromList
  , toList
  , apply

  -- * Basic functions
  , empty
  , singleton
  , cons
  , snoc
  , append
  , concat
  , replicate
  , list
  , head
  , tail
  , unfoldr
  , foldr
  , map
  , intercalate

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
  -- * Pattern Synonyms
  , pattern Nil
  , pattern Cons
#endif

  ) where

-- The 'Data.DList' module exists only to export names from 'Data.DList.Unsafe'.
-- Some names conflict with 'Prelude', so we hide all imports from 'Prelude'.
import Prelude ()

import Data.DList.Unsafe
