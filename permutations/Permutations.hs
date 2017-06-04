module Permutations where

import Data.List (tails,inits,nub)

interleave :: a -> [a] -> [[a]]
interleave x xs =  map (\(a,b) -> a ++ [x] ++ b) $ zip (inits xs) (tails xs)

permutations :: String -> [String]
permutations [] = [[]]
permutations (x:xs) = nub  $ concatMap (interleave x) (permutations xs)
