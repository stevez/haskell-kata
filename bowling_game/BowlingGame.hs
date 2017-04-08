module BowlingGame where
import Data.Char

score :: String -> Int
score [] = 0
score [_] = 0
score ['X',y,z] = 10 +  parsePin y + parsePin z
score (x:y:xs)
  | y == '/' = 10 + parsePin (head xs)  + score xs
  | x == 'X' =  10 + parsePin y + parsePin (head xs) + score (y:xs)
  | otherwise  = parsePin x + parsePin y + score xs

parsePin :: Char -> Int
parsePin '-' = 0
parsePin 'X' = 10
parsePin  c  = digitToInt c
