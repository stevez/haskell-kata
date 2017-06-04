import Data.List

permutation :: (Eq a) => Int -> [a] -> [[a]]
permutation 0 _ = [[]]
permutation n xs = [x:ys | x <- xs, ys <- permutation (n-1) (delete x xs)]


pick :: [a] -> Int ->  [(a,[a])]
pick [] _ = []
pick _  0 = []
pick (x:xs) n = (x,xs) : pick (xs++[x]) (n-1)

permutation' :: (Eq a) => Int -> [a] -> [[a]]
permutation' 0 _ = [[]]
permutation' n xs = do
         (x,rest) <- pick xs (length xs)
         ys <- permutation' (n-1) rest
         return $ x:ys
