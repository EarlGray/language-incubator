{-
-   See: http://matt.might.net/articles/cek-machines/
-}

type Var = String

data Lambda = Var :=> Exp deriving (Read, Show)

data Exp
    = Ref Var
    | Lam Lambda
    | Exp :@ Exp
  deriving (Read, Show)

type Program = Exp
type State  = (Exp, Env, Kont)
data D      = Clo (Lambda, Env)
type Env    = Var -> D
data Kont
    = Mt
    | Ar (Exp, Env, Kont)
    | Fn (Lambda, Env, Kont)
  deriving (Read, Show)



(==>) :: a -> b -> (a, b)
(==>) = (,)

(//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
(//) f [(x, y)] = \x' -> if x == x' then y else f x'

step :: State -> State
step (Ref x, e, k)
    = (Lam lam, e', k) where Clo (lam, e') = e x

step (f :@ x, e, k)
    = (f, e, Ar(x, e, k))

step (Lam lam, e, Ar(x, e', k))
    = (x, e', Fn (lam, e, k))

step (Lam lam, e, Fn(x :=> m, e', k))
    = (m, e' // [(x, Clo (lam, e))], k)
    

terminal :: (State -> State) -> (State -> Bool) -> State -> State
terminal step isFinal s 
    | isFinal s = s
    | otherwise = terminal step isFinal (step s)

inject :: Program -> State
inject m = (m, e0, Mt)
  where e0 = \x -> error $ "no binding for " ++ x 

isFinal :: State -> Bool
isFinal (Lam _, e, Mt)  = True
isFinal _               = False

evaluate :: Program -> State
evaluate = terminal step isFinal . inject

main = do
    exp <- read
    print . evaluate $ exp
    
