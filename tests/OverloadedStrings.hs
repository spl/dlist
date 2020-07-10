{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

-- | Tests using the OverloadedStrings language extension.
module OverloadedStrings (test) where

--------------------------------------------------------------------------------

import Data.DList

--------------------------------------------------------------------------------

test :: IO ()
test = print $ "OverloadedStrings:" `append` " success"
