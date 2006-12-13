
import qualified Prelude as P
import Prelude          hiding (concat)
import Data.List        hiding (concat)

import Parallel
import Data.DList

type T = [Int]

prop_model x = (toList . fromList $ (x :: T)) == id x

prop_empty = ([] :: T) == (toList empty :: T)

prop_singleton c = ([c] :: T) == toList (singleton c)

prop_cons c xs = (c : xs :: T) == toList (cons c (fromList xs))

prop_snoc xs c = (xs ++ [c] :: T) == toList (snoc (fromList xs) c)

prop_append xs ys = (xs ++ ys :: T) == toList (append (fromList xs) (fromList ys))

prop_concat zss  = (P.concat zss) == toList (concat (map fromList zss))
    where _ = zss :: [T]

main = pRun 2 500 [ ("model",     pDet prop_model)
                  , ("empty",     pDet prop_empty)
                  , ("singleton", pDet prop_singleton)
                  , ("cons",      pDet prop_cons)
                  , ("snoc",      pDet prop_snoc)
                  , ("append",    pDet prop_append)
                  , ("concat",    pDet prop_concat)
                  ]

