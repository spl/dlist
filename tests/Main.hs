-- | Test runner.
module Main (main) where

--------------------------------------------------------------------------------

import qualified DListProperties
import qualified OverloadedStrings

--------------------------------------------------------------------------------

main :: IO ()
main = do
  DListProperties.test
  OverloadedStrings.test
