module NewBowling where

import Data.Char

type Throw = Int

data Frame =  Open Throw Throw
            | Spare Throw
            | Strike
            | Bonus Throw
            deriving (Eq,Show)


parse :: String -> [Frame]
parse  [] = []
-- bonus throw if the last frame is spare
parse [x] = [Bonus (parseThrow x)]

-- 2 bonus throws if the last frame is strike
parse ['X', x,y]
  | y == '/' =  [Strike , Bonus (parseThrow x) , Bonus (10 - parseThrow x)]
  | otherwise = [Strike , Bonus (parseThrow x) , Bonus (parseThrow y) ]

parse (x:y:xs)
 | x == 'X' = Strike : parse (y:xs)
 | y == '/' = Spare  (parseThrow x) : parse xs
 | otherwise = Open (parseThrow x)  (parseThrow y) : parse xs

parseThrow :: Char -> Int
parseThrow '-' = 0
parseThrow 'X' = 10
parseThrow  c  = digitToInt c

score :: String -> Int
score xs = let frames = parse xs
           in calculate frames

calculate :: [Frame] -> Int
calculate [] = 0
calculate (Bonus _: xs) = 0 + calculate xs
calculate (Open x y : xs) = x + y + calculate xs
calculate (Spare _ : xs)  = 10 + spareBonus xs  + calculate xs
calculate (Strike : xs)   = 10 + strikeBonus xs  + calculate xs

spareBonus :: [Frame] -> Int
spareBonus xs = sum $ take 1 $ toThrow xs

strikeBonus :: [Frame] -> Int
strikeBonus xs = sum $ take 2 $ toThrow xs


toThrow :: [Frame] -> [Throw]
toThrow [] = []
toThrow (Strike : xs) = 10 : toThrow xs
toThrow (Open x y : xs) = x : y : toThrow xs
toThrow (Spare x : xs)  = x : (10-x) :toThrow xs
toThrow (Bonus x : xs)  = x : toThrow xs
