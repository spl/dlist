{-# LANGUAGE OverloadedStrings #-}

module OverloadedStrings (testOverloadedStrings) where

import Data.DList

testOverloadedStrings :: IO ()
testOverloadedStrings = print $ "OverloadedStrings:" `append` " success"
