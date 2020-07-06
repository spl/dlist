-- Options passed to Haddock
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708

-- The 'Data.DList.DNonEmpty.Unsafe' module exports 'UnsafeDNonEmpty' and
-- 'unsafeApplyDNonEmpty', which allow breaking the invariant of the 'DNonEmpty'
-- newtype. Therefore, we explicitly mark 'Data.DList.DNonEmpty.Unsafe' as
-- unsafe.
{-# LANGUAGE Unsafe #-}
#endif

-----------------------------------------------------------------------------

{-|

Module: Data.DList.DNonEmpty.Unsafe
Copyright: © 2020 Sean Leather
License: BSD-3-Clause

Maintainer: sean.leather@gmail.com
Stability: stable

This module exports the 'DNonEmpty' constructor, 'UnsafeDNonEmpty', and the
record label, 'unsafeApplyDNonEmpty', both of which can be used to create unsafe
'DNonEmpty' values that break the invariant preserved by the names exported from
'Data.DList.DNonEmpty'.

-}

module Data.DList.DNonEmpty.Unsafe (DNonEmpty (UnsafeDNonEmpty, unsafeApplyDNonEmpty)) where

import Data.DList.DNonEmpty.Internal
