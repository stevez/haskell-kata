import Control.Monad


data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

bools :: Int -> [[Bool]]
bools n = replicateM n [True,False]

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And x y) = vars x ++ vars y
vars (Imply x y) = vars x ++ vars y

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const x) = x
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p1 p2) = (eval s p1) && (eval s p2)
eval s (Imply p1 p2) = (eval s p1) <= (eval s p2)


rmdups :: (Eq a) => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

substs :: Prop -> [Subst]
substs p =  do
    let vars' = rmdups (vars p)
    boolChoice <- bools (length vars')
    return $ zip vars' boolChoice

--
isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]


--
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
