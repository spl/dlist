-- Options passed to GHC
{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import qualified DListProperties
import qualified DNonEmptyProperties
import qualified OverloadedStrings

--------------------------------------------------------------------------------

main :: IO ()
main = do
  DListProperties.test
  DNonEmptyProperties.test
  OverloadedStrings.test
