module MinesSweeper where

import Data.List.Split
import Data.Char

solve :: Int -> Int -> [String] -> [String]
solve row col xs = let mf = toMineField col xs
                       field = sweepMineField row col xs mf
                    in chunksOf col field


sweepMineField :: Int -> Int -> [String] -> MineField -> String
sweepMineField _ _ _ []  = []
sweepMineField row col xs ((x,y,c) : mf)
 | c == '*' = c : sweepMineField row col xs mf
 | otherwise = calcMines x y row col xs : sweepMineField row col xs mf


calcMines :: Int -> Int -> Int -> Int -> [String] -> Char
calcMines x y row col xs = do
   let ns = neighbours row col (x,y)
   let fields = map (\(i,j) -> xs !! i !! j) ns
   let mines = length $ filter (=='*') fields
   intToDigit mines


type MineField = [(Int,Int,Char)]

toMineField :: Int -> [String] -> MineField
toMineField col xs =  map (\(i,c) -> (i `div` col, i `mod` col, c)) $ zip [0..] (concat xs)

neighbours :: Int -> Int -> (Int,Int) -> [(Int,Int)]
neighbours row col (x,y) = [(a,b) | a <- [x-1,x,x+1],
                                    b <-[y-1,y,y+1],
                                    a >= 0 && a < row,
                                    b >= 0 && b < col,
                                    (a,b) /= (x,y)]
