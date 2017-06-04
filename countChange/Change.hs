module Change where
import Data.List

countChange :: Integer -> [Integer] -> Integer
countChange 0 _ = 1
countChange n [x] = if n `mod` x == 0 then 1 else 0
countChange n (x:xs) = sum [ countChange (n - m*x) xs  | m <- [0..(n `div` x)] ]


countChange' :: Integer -> [Integer] -> Integer
countChange' n [] = 0
countChange' n (x:xs)
  | n > 0 =  countChange' (n - x) (x:xs) + countChange' n xs
  | n == 0 = 1
  | n < 0 = 0
