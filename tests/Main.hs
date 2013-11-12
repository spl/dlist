module Main (main) where

--------------------------------------------------------------------------------

import Control.Monad (unless)
import System.Exit (exitFailure)

import Properties (run)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  success <- run
  unless success exitFailure

