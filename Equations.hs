import Data.List
import Data.Maybe
import Control.Monad

--split :: [a] -> [([a],[a])]
--split xs = map (`splitAt` xs) [1..(length xs -1)]

split :: [a] -> [([a],[a])]
split xs =  tail $ init $ zip (inits xs) (tails xs)

data Op = Add | Sub | Mul | Div deriving (Enum,Bounded,Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

type Value = Rational

apply :: Op -> Value -> Value -> Maybe Value
apply Add x y = Just (x + y)
apply Sub x y = Just (x - y)
apply Mul x y = Just (x * y)
apply Div _ 0 = Nothing
apply Div x y = Just (x / y)

data Expr = Val Integer | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                      where
                        brak (Val n) = show n
                        brak e = "(" ++ show e ++ ")"
data Equation = Equation Expr Expr

instance Show Equation where
  show (Equation el er) = show el ++ "=" ++ show er

choices :: [a] -> [([a],[a])]
choices = split

type Result = (Expr, Value)

results :: [Integer] -> [Result]
results [] = []
results [n] = [(Val n, fromInteger n)]
results ns = [result | (ls,rs) <- split ns,
                        rl <- results ls,
                        rr <- results rs,
                        result <- combine rl rr]
--
combine :: Result -> Result -> [Result]
combine (el,nl) (er,nr)  = do
     o <- [minBound..maxBound]
     guard (not (right_associative o er))  
     v <- maybeToList (apply o nl nr)
     return (App o el er, v)

--
-- e1 op (e2 op' e3) == (e1 op e2) op' e3
right_associative :: Op -> Expr -> Bool
right_associative Add (App Add _ _) = True
right_associative Add (App Sub _ _) = True
right_associative Mul (App Mul _ _) = True
right_associative Mul (App Div _ _) = True
right_associative _ _ = False

solutions :: [Integer] -> [Equation]
solutions ns = [Equation el er |  (xs,ys) <- choices ns,
                                   (el,vl) <- results xs,
                                   (er,vr) <- results ys,
                                   vl == vr]

--
main :: IO ()
main = mapM_ print $ solutions [2,3,5,7,11]
