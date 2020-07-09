{- ORMOLU_DISABLE -}
-- Options passed to GHC
{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{- ORMOLU_ENABLE -}

--------------------------------------------------------------------------------

-- | Tests using the OverloadedStrings language extension.
module OverloadedStrings (test) where

--------------------------------------------------------------------------------

import qualified Data.DList as DList
import qualified Data.DList.DNonEmpty as DNonEmpty

--------------------------------------------------------------------------------

test :: IO ()
test = do
  print $ "OverloadedStrings for DList:     " `DList.append` "success"
  print $ "OverloadedStrings for DNonEmpty: " `DNonEmpty.append` "success"
