module Main (main) where

import Control.Monad.Trans.Writer.Lazy (runWriter, tell)
import Criterion.Main (bench, bgroup, defaultMain, whnf)
import qualified Data.DList as DList
import qualified Data.DList.DNonEmpty as DNonEmpty
import qualified Data.DList.NonEmpty as NonEmptyDList
import qualified Data.Foldable as Foldable
import qualified Data.List.NonEmpty as NonEmpty
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup
        "append"
        [ bench "List" $
            whnf (append 1000) $
              [1, 2, 3, 4, 5],
          bench "DList" $
            whnf (append 1000) $
              DList.fromList [1, 2, 3, 4, 5],
          bench "NonEmpty" $
            whnf (append 1000) $
              1 NonEmpty.:| [2, 3, 4, 5],
          bench "DNonEmpty" $
            whnf (append 1000) $
              DNonEmpty.fromNonEmpty $ 1 NonEmpty.:| [2, 3, 4, 5],
          bench "NonEmptyDList" $
            whnf (append 1000) $
              NonEmptyDList.fromNonEmpty $ 1 NonEmpty.:| [2, 3, 4, 5]
        ],
      bgroup
        "fmap_append"
        [ bench "List" $
            whnf (fmap_append 1000) $
              [1, 2, 3, 4, 5],
          bench "DList" $
            whnf (fmap_append 1000) $
              DList.fromList [1, 2, 3, 4, 5],
          bench "NonEmpty" $
            whnf (fmap_append 1000) $
              1 NonEmpty.:| [2, 3, 4, 5],
          bench "DNonEmpty" $
            whnf (fmap_append 1000) $
              DNonEmpty.fromNonEmpty $ 1 NonEmpty.:| [2, 3, 4, 5],
          bench "NonEmptyDList" $
            whnf (fmap_append 1000) $
              NonEmptyDList.fromNonEmpty $ 1 NonEmpty.:| [2, 3, 4, 5]
        ],
      bgroup
        "writer"
        [ bench "List" $
            whnf (writer (: [])) $
              Branch (Branch (Leaf 'a') (Leaf 'b')) (Branch (Leaf 'c') (Leaf 'd')),
          bench "DList" $
            whnf (writer DList.singleton) $
              Branch (Branch (Leaf 'a') (Leaf 'b')) (Branch (Leaf 'c') (Leaf 'd'))
        ]
    ]

-- | Left-nested append
append :: (Semigroup (f Int), Foldable f) => Int -> f Int -> Int
append m right = Foldable.foldl' (+) 0 $ Foldable.toList $ go m right
  where
    go n left
      | n <= 0 = left
      | otherwise = go (pred n) (left <> right)

-- | Left-nested append with map
fmap_append :: (Foldable f, Functor f, Semigroup (f Int)) => Int -> f Int -> Int
fmap_append m x = Foldable.foldl' (+) 0 $ Foldable.toList $ go m x
  where
    go n y
      | n <= 0 = y
      | otherwise = go (pred n) (fmap (+ 1) $ y <> x)

data Tree a = Leaf a | Branch (Tree a) (Tree a)

-- | Tree flattening with the Writer monad
writer :: Monoid w => (Char -> w) -> Tree Char -> w
writer f = snd . runWriter . go
  where
    go (Leaf x) = tell (f x)
    go (Branch x y) = go x >> go y
