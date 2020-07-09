-- Options passed to GHC
{-# OPTIONS_GHC -Wall #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import qualified DListProperties
import qualified OverloadedStrings

--------------------------------------------------------------------------------

main :: IO ()
main = do
  OverloadedStrings.test
  DListProperties.test
