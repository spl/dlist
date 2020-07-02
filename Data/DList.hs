-- Options passed to Haddock
{-# OPTIONS_HADDOCK prune #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

-- GHC >= 7.8
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708
-- The 'Data.DList' module exports only the safe aspects of 'Data.DList.Unsafe'.
-- Specifically, it does not export the 'DList' constructor 'UnsafeDList' or
-- record label 'unsafeFromDList'. Therefore, we mark 'Data.DList' as
-- trustworthy.
{-# LANGUAGE Trustworthy #-}
#endif

-- GHC >= 7.8 && <= 8
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
-- Required for 'pattern' in the export list.
{-# LANGUAGE PatternSynonyms #-}
#endif

-----------------------------------------------------------------------------

-- |
-- Module: Data.DList
-- Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
-- License: BSD-3-Clause
--
-- Maintainer: sean.leather@gmail.com
-- Stability: stable
-- Portability: portable
--
-- Difference lists: a data structure for /O(1)/ append on lists.
module Data.DList
  ( -- * Type
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 800
    DList(Nil, Cons),
#else
    DList,
#endif

    -- * Conversion
    fromList,
    toList,
    apply,

    -- * Basic functions
    empty,
    singleton,
    cons,
    snoc,
    append,
    concat,
    replicate,
    list,
    head,
    tail,
    unfoldr,
    foldr,
    map,
    intercalate,

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
    -- * Pattern Synonyms
    pattern Nil,
    pattern Cons,
#endif
  )
where

-----------------------------------------------------------------------------

import Data.DList.Unsafe
-- The 'Data.DList' module exists only to export names from 'Data.DList.Unsafe'.
-- Some names conflict with 'Prelude', so we hide all imports from 'Prelude'.
import Prelude ()
