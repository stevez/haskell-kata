module CountDown where

import Control.Monad
import Data.List

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = x <= y
valid' Sub x y = x > y
valid' Mul x y = x /= 1 && y /= 1 && x <= y
valid' Div x y = y /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                      where
                        brak (Val n) = show n
                        brak e = "(" ++ show e ++ ")"

--
values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [ n | n > 0]
eval (App o l r) = [ apply o el er | el <- eval l,
                                     er <- eval r,
                                     valid o el er]

{--
choices :: [a] -> [[a]]
choices xs =  do
   sub <- subsequences xs
   permutations sub
--}

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

choices' :: [a] -> [[a]]
choices' [] = []
choices' xs = [choice | sub <- subs xs, choice <- perms sub ]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                 l       <- exprs ls,
                 r       <- exprs rs,
                 e       <- combine l r]

--
--
combine :: Expr -> Expr -> [Expr]
combine el er  = [App o el er | o <- ops]

type Result = (Expr,Int)

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [result | (ls,rs) <- split ns,
                        rl <- results ls,
                        rr <- results rs,
                        result <- combine' rl rr]

--
combine' :: Result -> Result -> [Result]
combine' (el,nl) (er,nr)  = [ (App o el er, apply o nl nr) | o <- ops, valid o nl nr]


ops :: [Op]
ops = [Add,Sub,Mul,Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e, n') <- results ns', n' == n ]


results' :: [Int] -> [Result]
results' [] = []
results' [n] = [(Val n, n) | n > 0]
results' ns = [result | (ls,rs) <- split ns,
                        rl <- results' ls,
                        rr <- results' rs,
                        result <- combine'' rl rr]

--
combine'' :: Result -> Result -> [Result]
combine'' (el,nl) (er,nr)  = [ (App o el er, apply o nl nr) | o <- ops, valid' o nl nr]

--
solutions'' :: [Int] -> Int -> [Expr]
solutions'' ns n = [e | ns' <- choices ns, (e, n') <- results' ns', n' == n]


main :: IO ()
--main = print (solutions [1,3,7,10,25,50] 765)
--main = print (solutions' [1,3,7,10,25,50] 765)
main = print (solutions'' [1,3,7,10,25,50] 765)
