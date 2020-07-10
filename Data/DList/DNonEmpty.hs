{- ORMOLU_DISABLE -}
{-|

Module: Data.DList.DNonEmpty
Copyright: © 2017-2020 Oleg Grenrus, 2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

A __difference list__ is an abstraction representing a list that
supports&#x00A0;\(\mathcal{O}\)(@1@) 'append' and 'snoc' operations. This module
provides the type for a difference list, 'DNonEmpty', and a collection of
supporting functions for (a) converting to and from lists and (b) operating on
'DNonEmpty's efficiently.

-}
{- ORMOLU_ENABLE -}

module Data.DList.DNonEmpty
  ( -- * Non-Empty Difference List Type
    DNonEmpty((:|)),

    -- * Conversion
    fromNonEmpty,
    toNonEmpty,
    toDList,

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

import Data.DList.DNonEmpty.Internal

{- ORMOLU_DISABLE -}
{-

The 'Data.DList.DNonEmpty' module exists only to export names from
'Data.DList.DNonEmpty.Internal'. Some names conflict with 'Prelude', so we hide
all imports from 'Prelude'.

-}
{- ORMOLU_ENABLE -}
import Prelude ()
