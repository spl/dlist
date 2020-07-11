{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

--------------------------------------------------------------------------------

-- | Test runner.
module Main (main) where

--------------------------------------------------------------------------------

import qualified DListProperties
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
import qualified DNonEmptyProperties
#endif
import qualified OverloadedStrings

--------------------------------------------------------------------------------

main :: IO ()
main = do
  DListProperties.test
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
  DNonEmptyProperties.test
#endif
  OverloadedStrings.test
