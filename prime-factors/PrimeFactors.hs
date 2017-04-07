module PrimeFactors where

primeFactors :: Int -> [Int]
primeFactors n = primeFactorsHelper n 2

primeFactorsHelper :: Int -> Int -> [Int]
primeFactorsHelper 1 _ = []
primeFactorsHelper n candidate
  | n `mod` candidate == 0 = candidate : primeFactorsHelper (n `div` candidate) candidate
  | otherwise =  primeFactorsHelper n (candidate + 1)
