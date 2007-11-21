
import qualified Prelude   as P
import qualified Data.List as P (unfoldr)
import Prelude          hiding (concat,map,head,tail,foldr,map)
import Data.List        hiding (concat,map,head,tail,unfoldr,foldr,map)
import Text.Show.Functions

import Parallel
import Data.DList

type T = [Int]

prop_model x = (toList . fromList $ (x :: T)) == id x

prop_empty = ([] :: T) == (toList empty :: T)

prop_singleton c = ([c] :: T) == toList (singleton c)

prop_cons c xs = (c : xs :: T) == toList (cons c (fromList xs))

prop_snoc xs c = (xs ++ [c] :: T) == toList (snoc (fromList xs) c)

prop_append xs ys = (xs ++ ys :: T) == toList (append (fromList xs) (fromList ys))

prop_concat zss  = (P.concat zss) == toList (concat (P.map fromList zss))
    where _ = zss :: [T]

prop_head xs = not (null xs) ==> (P.head xs) == head (fromList xs)
    where _ = xs :: T

prop_tail xs = not (null xs) ==> (P.tail xs) == (toList . tail . fromList) xs
    where _ = xs :: T

prop_unfoldr f x n = n >= 0 ==> take n (P.unfoldr f x)
                             == take n (toList $ unfoldr f x)
    where _ = x :: Int
          _ = f :: Int -> Maybe (Int,Int)

prop_foldr f x xs = (P.foldr f x xs) == (foldr f x (fromList xs))
    where _ = x :: Int
          _ = f :: Int -> Int -> Int

prop_map f xs = (P.map f xs) == (toList $ map f (fromList xs))
    where _ = f :: Int -> Int

prop_map_fusion f g xs = (P.map f . P.map g $ xs)
                      == (toList $ map f . map g $ fromList xs)
    where _ = f :: Int -> Int

--
-- run 8 threads simultaneously
--
main = pRun 8 300
    [ ("model",     pDet prop_model)
    , ("empty",     pDet prop_empty)
    , ("singleton", pDet prop_singleton)
    , ("cons",      pDet prop_cons)
    , ("snoc",      pDet prop_snoc)
    , ("append",    pDet prop_append)
    , ("concat",    pDet prop_concat)
    , ("head",      pDet prop_head)
    , ("tail",      pDet prop_tail)
    , ("unfoldr",   pDet prop_unfoldr)
    , ("foldr",     pDet prop_foldr)
    , ("map",       pDet prop_map)
    , ("map fusion",pDet prop_map)
    ]


------------------------------------------------------------------------
--
-- missing QC instances
--

{-
instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary           = do a <- arbitrary ; elements [Nothing, Just a]
  coarbitrary Nothing = variant 0
  coarbitrary _       = variant 1 -- ok?
      -}
