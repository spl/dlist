{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.DList.Instances () where

import Data.DList
import Data.Semigroup

instance Semigroup (DList a)
