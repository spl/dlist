{-# LANGUAGE CPP #-}

-- CPP: GHC >= 7.8 for Safe Haskell
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Safe #-}
#endif

--------------------------------------------------------------------------------

-- | Test runner.
module Main (main) where

--------------------------------------------------------------------------------

import qualified DListProperties
import qualified DNonEmptyProperties
import qualified OverloadedStrings
import QuickCheckUtil (quickCheckLabeledProperties)
import Control.Monad (unless)
import Test.QuickCheck (isSuccess)
import System.Exit (exitFailure)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  result <- quickCheckLabeledProperties $
    DListProperties.properties
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
    ++ DNonEmptyProperties.properties
#endif
  OverloadedStrings.test
  unless (isSuccess result) exitFailure
