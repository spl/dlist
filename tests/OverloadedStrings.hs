{- ORMOLU_DISABLE -}
-- Options passed to GHC
{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}

#if !defined(__GLASGOW_HASKELL__)
#error "Your compiler is not GHC. Let us know if dlist can be made to work on it."
#endif

{-# LANGUAGE OverloadedStrings #-}
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------

-- | Tests using the OverloadedStrings language extension.
module OverloadedStrings (test) where

--------------------------------------------------------------------------------

import qualified Data.DList as DList
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
import qualified Data.DList.DNonEmpty as DNonEmpty
#endif

--------------------------------------------------------------------------------

test :: IO ()
test = do
  print $ "OverloadedStrings for DList:     " `DList.append` "success"
-- CPP: GHC >= 8 for DNonEmpty
#if __GLASGOW_HASKELL__ >= 800
  print $ "OverloadedStrings for DNonEmpty: " `DNonEmpty.append` "success"
#endif
