module PascalsTriangle where

pascalsTriangle :: Int -> [Int]
pascalsTriangle 1 = [1]
pascalsTriangle n = pascalsTriangle (n-1) ++ row n

row :: Int -> [Int]
row 1 = [1]
row n = [1] ++ zipWith (+) s (tail s) ++ [1]
        where s = row (n-1)

nextRow row = [1] ++ zipWith (+) row (tail row) ++ [1]
