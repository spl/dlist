-- Options passed to Haddock
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708

-- The 'Data.DList.Unsafe' module exports 'UnsafeDList' and 'unsafeApplyDList',
-- which allow breaking the invariant of the 'DList' newtype. Therefore, we
-- explicitly mark 'Data.DList.Unsafe' as unsafe.
{-# LANGUAGE Unsafe #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.Unsafe
Copyright: © 2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

This module exports the 'DList' constructor, 'UnsafeDList', and the record label,
'unsafeApplyDList', both of which can be used to create unsafe 'DList' values
that break the invariant preserved by the names exported from 'Data.DList'.

-}

module Data.DList.Unsafe (DList (UnsafeDList, unsafeApplyDList)) where

import Data.DList.Internal (DList (UnsafeDList, unsafeApplyDList))
