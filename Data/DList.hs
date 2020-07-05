{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
-- The 'Data.DList' module exports only the safe aspects of 'Data.DList.Unsafe'.
-- Specifically, it does not export the 'DList' constructor 'UnsafeDList' or
-- record label 'unsafeApplyDList'. Therefore, we mark 'Data.DList' as
-- trustworthy.
{-# LANGUAGE Trustworthy #-}
#endif

-- CPP: GHC >= 7.8 && <= 8 for 'pattern' required in the export list
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
{-# LANGUAGE PatternSynonyms #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList
Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

A __difference list__ is an abstraction representing a list that
supports&#x00A0;\(\mathcal{O}\)(@1@) 'append' and 'snoc' operations. This module
provides the type for a difference list, 'DList', and a collection of supporting
functions for (a) converting to and from lists and (b) operating on 'DList's
efficiently.

-}

module Data.DList
  ( -- * Difference List Type
    DList

-- CPP: GHC >= 8 for pattern synonyms allowed in the constructor
#if __GLASGOW_HASKELL__ >= 800
    (Nil, Cons),
#else
    ,
-- CPP: GHC >= 7.8 && <= 8 for 'pattern' required in the export list
#if __GLASGOW_HASKELL__ >= 708
    -- ** Bundled Patterns
    pattern Nil,
    pattern Cons,
#endif
#endif

    -- * Conversion
    fromList,
    toList,
    apply,

    -- * Basic Functions
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
  )
where

-----------------------------------------------------------------------------

import Data.DList.Unsafe

-- The 'Data.DList' module exists only to export names from 'Data.DList.Unsafe'.
-- Some names conflict with 'Prelude', so we hide all imports from 'Prelude'.
import Prelude ()
