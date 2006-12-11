
import Parallel
import Data.List
import Data.DList

type T = [Int]

prop_model x = (toList . fromList $ (x :: T)) == id x

prop_singleton c = ([c] :: T) == toList (singleton c)

prop_snoc xs c = (xs ++ [c] :: T) == toList (snoc (fromList xs) c)

prop_append xs ys = (xs ++ ys :: T) == toList (append (fromList xs) (fromList ys))

main = pRun 2 1000 [ ("model",     pDet prop_model)
                   , ("singleton", pDet prop_singleton)
                   , ("snoc",      pDet prop_snoc)
                   , ("append",    pDet prop_append)
                   ]

