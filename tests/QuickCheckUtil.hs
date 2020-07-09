{- ORMOLU_DISABLE -}
-- Options passed to GHC
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------

-- | QuickCheck utilities for testing.
module QuickCheckUtil where

--------------------------------------------------------------------------------

-- CPP: base >= 4.9 for NonEmpty
#if MIN_VERSION_base(4,9,0)
import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe (mapMaybe)
#endif
import Test.QuickCheck

--------------------------------------------------------------------------------

eqWith :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
eqWith f g x = f x == g x

eqOn :: Eq b => (a -> Bool) -> (a -> b) -> (a -> b) -> a -> Property
eqOn c f g x = c x ==> f x == g x

--------------------------------------------------------------------------------

-- CPP: base >= 4.9 for NonEmpty
#if MIN_VERSION_base(4,9,0)

{-

We include the instances for NonEmpty because QuickCheck (>= 2.10) does not. We
could alternatively depend on quickcheck-instances (>= 0.3.15), but
quickcheck-instances has sometimes lagged behind newer GHC/base versions. By
including the instances here, we do not need to track the quickcheck-instances
version, thus simplifying dlist.cabal and reducing the maintenance effort.

-}

instance Arbitrary1 NonEmpty where
  liftArbitrary arb = liftA2 (:|) arb (liftArbitrary arb)
  liftShrink shr (x :| xs) = mapMaybe nonEmpty . liftShrink shr $ x : xs

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = arbitrary1
  shrink = shrink1

#endif
