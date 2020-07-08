{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- CPP: GHC >= 7.8 && <= 8 for 'pattern' required in the export list
#if __GLASGOW_HASKELL__ >= 708 && __GLASGOW_HASKELL__ < 800
{-# LANGUAGE PatternSynonyms #-}
#endif

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-

The 'Data.DList.DNonEmpty' module imports the unsafe module
'Data.DList.DNonEmpty.Internal' but exports only its safe aspects. Specifically,
it does not export the 'DNonEmpty' constructor 'UnsafeDNonEmpty' or record label
'unsafeApplyDNonEmpty'. Therefore, we mark 'Data.DList.DNonEmpty' as
trustworthy.

-}
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.DNonEmpty
Copyright: © 2006-2009 Don Stewart, 2013-2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

A __difference list__ is an abstraction representing a list that
supports&#x00A0;\(\mathcal{O}\)(@1@) 'append' and 'snoc' operations. This module
provides the type for a difference list, 'DNonEmpty', and a collection of
supporting functions for (a) converting to and from lists and (b) operating on
'DNonEmpty's efficiently.

-}

module Data.DList.DNonEmpty
  ( -- * Non-Empty Difference List Type
    DNonEmpty

-- CPP: GHC >= 8 for pattern synonyms allowed in the constructor
#if __GLASGOW_HASKELL__ >= 800
    (Cons),
#else
    ,
-- CPP: GHC >= 7.8 && <= 8 for 'pattern' required in the export list
#if __GLASGOW_HASKELL__ >= 708
    -- ** Bundled Patterns
    pattern Cons,
#endif
#endif

    -- * Conversion
    fromNonEmpty,
    toNonEmpty,

    -- * Basic Functions
    singleton,
    cons,
    snoc,
    append,
    head,
    tail,
    unfoldr,
    map,
  )
where

-----------------------------------------------------------------------------

-- The 'Data.DList.DNonEmpty' module exists only to export names from
-- 'Data.DList.DNonEmpty.Internal'. Some names conflict with 'Prelude', so we
-- hide all imports from 'Prelude'.
import Prelude ()

import Data.DList.DNonEmpty.Internal
