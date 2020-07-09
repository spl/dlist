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

import Data.DList

--------------------------------------------------------------------------------

test :: IO ()
test = print $ "OverloadedStrings:" `append` " success"
