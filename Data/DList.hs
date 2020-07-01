{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE CPP #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
-- Mark this module as trustworthy even though we import 'IsList' from GHC.Exts,
-- which is marked unsafe. 'IsList' is safe.
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

import Data.DList.Unsafe

-- This module exists only to export names from 'Data.DList.Unsafe'. Some
-- conflict with 'Prelude', so we hide all names imported from 'Prelude'.
import Prelude ()
