module Yatzy where

import Data.List

chance :: [Int] -> Int
chance = sum

yatzy :: [Int] -> Int
yatzy xs = if isYatzy xs
           then 50
           else 0

ones :: [Int] -> Int
ones  = sum . filter (==1)

twos :: [Int] -> Int
twos = sum . filter (==2)

threes :: [Int] -> Int
threes = sum . filter (==3)

fours :: [Int] -> Int
fours =  sum . filter (==4)

fives :: [Int] -> Int
fives =  sum . filter (==5)

sixes :: [Int] -> Int
sixes = sum . filter (==6)

score_pair :: [Int] -> Int
score_pair  = maybe 0 (*2) . fmap maximum .  find_n_of_a_kind 2

two_pair :: [Int] -> Int
two_pair xs =  let pairs = find_n_of_a_kind 2 xs
               in case pairs of
                 Just [x,y] -> (x + y) * 2
                 _  -> 0


three_of_a_kind :: [Int] -> Int
three_of_a_kind =   maybe 0 (*3) . fmap head . find_n_of_a_kind 3

four_of_a_kind :: [Int] -> Int
four_of_a_kind = maybe 0 (*4) . fmap head . find_n_of_a_kind 4

find_n_of_a_kind :: Int -> [Int] -> Maybe [Int]
find_n_of_a_kind n xs  = let result =  (map fst . filter ((>= n) . snd) . countSameDice) xs
                         in case result of
                            [] -> Nothing
                            _  -> Just result

smallStraight :: [Int] -> Int
smallStraight xs = if isSmallStraight xs
                   then 15
                   else 0

largeStraight :: [Int] -> Int
largeStraight xs = if isLargeStraight xs
                   then 20
                   else 0

fullHouse :: [Int] -> Int
fullHouse xs
      | isFullHouse xs  = sum xs
      | otherwise = 0

isFullHouse :: [Int] -> Bool
isFullHouse  = ([2,3] ==) . sort . map snd . countSameDice

countSameDice :: [Int] -> [(Int,Int)]
countSameDice =  map ((,) <$> head <*> length) . group . sort

isYatzy :: [Int] -> Bool
isYatzy =  (==[5]) . map snd . countSameDice

isSmallStraight :: [Int] -> Bool
isSmallStraight = (==[1..5]) . sort

isLargeStraight :: [Int] -> Bool
isLargeStraight = (==[2..6]) . sort
