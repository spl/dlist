{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.DList
-- Copyright   :  (c) 2006-2009 Don Stewart, 2013-2016 Sean Leather
-- License     :  See LICENSE file
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

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
  -- * Pattern Synonyms
  , pattern Nil
  , pattern Cons
#endif

  ) where

import Prelude ()
import Data.DList.Internal
