module BowlingGame where
import Data.Char

score :: String -> Int
score [] = 0
score [_] = 0
score ['X',y,z] = 10  +  scoreOfPins y z
score ('X':y:z:xs) = 10 + scoreOfPins y z + score (y:z:xs)
score (_:'/':y:xs) = 10 + scoreOfPin y + score (y:xs)
score (x:y:xs) = scoreOfPins x y + score xs

scoreOfPin :: Char -> Int
scoreOfPin '-' = 0
scoreOfPin 'X' = 10
scoreOfPin  c  = digitToInt c

scoreOfPins :: Char -> Char -> Int
scoreOfPins _ '/' = 10
scoreOfPins x y  = scoreOfPin x + scoreOfPin y
