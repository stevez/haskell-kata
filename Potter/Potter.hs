module Potter where

import Data.List
import Data.Ord (comparing)

unitPrice :: Double
unitPrice = 8.0

-- old function
{--
price :: (Ord a) => [a] -> Double
price [] = 0
price xs = let (g,rest) = groupBooks xs
           in groupPrice g + price rest
--}

--
price :: [Int] -> Double
price [] = 0.0
price xs = totalPrice $ lowestPriceGroup xs


groupPrice :: [a] -> Double
groupPrice xs = case length xs of
                1 -> unitPrice
                2 -> unitPrice * 2 * 0.95
                3 -> unitPrice * 3 * 0.9
                4 -> unitPrice * 4 * 0.8
                5 -> unitPrice * 5 * 0.75

groupBooks :: (Ord a) => [a] -> ([a],[a])
groupBooks [] = ([],[])
groupBooks xs = let g = map head $ group $ sort xs
                    r = concatMap tail (group xs)
                in (g,r)


exclude :: (Eq a) => [a] -> [a] -> [a]
exclude [] xs = xs
exclude _  [] = []
exclude (x:xs) ys = exclude xs (delete x ys)

-- too many options --
groups :: (Eq a) => [a] -> [[[a]]]
groups [] = [[]]
groups xs = do
        sub <- [ x | x <- subsequences xs, not (null x), length x <=5 ]
        rest <- groups (exclude sub xs)
        [sub : rest]

choices :: (Ord a) => [a] -> [[[a]]]
choices [] = [[]]
choices xs = do
      let unique = map head $ group $ sort xs
      k <- [1..(min (length unique) 5)]
      let sub = take k unique
      rest <- choices (exclude sub xs)
      [sub : rest]

totalPrice :: [[a]] -> Double
totalPrice xs = sum $ map groupPrice xs

lowestPriceGroup :: (Ord a) => [a] -> [[a]]
lowestPriceGroup xs = minimumBy (comparing totalPrice) $ choices xs
