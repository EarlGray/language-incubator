{-
-   See: http://matt.might.net/articles/cek-machines/
-}
import qualified Data.Map as M

type Var = String
data Val 
    = VInt Int
    | VClo Lam Env
    | VLam Lam
  deriving (Show, Read, Eq)

data Lam = Var :=> Exp 
  deriving (Read, Show, Eq)

data Exp
    = MRef Var
    | MVal Val
    | MAp Exp Exp
  deriving (Read, Show, Eq)

type Program = Exp
type State   = (Exp, Env, Kont)
data Kont
    = KStart
    | KEmpty
    | KHoleFun (Exp, Env) Kont
    | KHoleArg Val Kont
    | KEnd
  deriving (Read, Show)

type Env = M.Map Var Val

{-
 - Environment
 -}
emptyEnv :: Env
emptyEnv = M.empty

extendEnv :: (Var, Val) -> Env -> Env
extendEnv (var, val) e = M.insert var val e

lookupEnv :: Var -> Env -> Val
lookupEnv v e = 
    case M.lookup v e of
      Just r -> r
      Nothing -> error $ "Lookup of '" ++ v ++ "' failed in E{" ++ show e ++ "}"

{-
 - CEK machine
 -}
step :: State -> State
step (MRef x,         e,  k) = (MVal v, e, k)
    where v = lookupEnv x e
step (MAp m1 m2,      e,  k) = (m1, e, k')
    where k' = KHoleFun (m2, e) k
step (MVal (VLam lam), e, k) = (MVal clo, e, k)
    where clo = VClo lam e
step (MVal w, _, KHoleFun (m, e) k) = (m, e, KHoleArg w k)
step (MVal w, _, KHoleArg (VClo (x :=> m) e) k) = (m, extendEnv (x, w) e, k)
step (w, e, _) = (w, e, KEnd)

run :: State -> State
run (exp, env, KEnd) = (exp, env, KEnd)
run (exp, env, KStart) = run $ step (exp, env, KEmpty)
run (exp, env, k)      = run $ step (exp, env, k)

evaluate :: Program -> Exp
evaluate exp = 
    case run (exp, emptyEnv, KStart) of
      (result, _, KEnd) -> result
      st -> error $ "Program exited with invalid state " ++ show st

{-
 - for GHCi
 -}
lam = MVal . VLam
int = MVal . VInt
ref = MRef
(<.>) = MAp
infixl 9 <.> 

test_id = lam ("x":=>ref "x")
test_int = int 42
test1 = test_id <.> test_int
test2 = ((lam("x":=>(lam("y":=>ref "x")))) <.> (int 1)) <.> (int 2)

p0 = lam("f":=>lam("x":=>ref "x"))
p1 = lam("f":=>lam("x":=>(ref "f" <.> ref "x")))
psucc = lam("n":=>(
          lam ("f" :=>(
            lam ("x" :=>(
              ref "f" <.> (ref "n" <.> ref "f" <.> ref "x")))))))
p2 = psucc <.> p1


pretty :: Exp -> String
pretty (MRef x) = x
pretty (MAp m1 m2) = pm1 ++ " " ++ pm2
    where pm1 = bracify1 m1 $ m1
          pm2 = bracify2 m2 $ m2
          bracify1 (MRef _) m = pretty m
          bracify1 (MAp _ _) m = pretty m
          bracify1 _ m = "(" ++ pretty m ++ ")"

          bracify2 (MRef _) m = pretty m
          bracify2 (MVal (VInt _)) m = pretty m
          bracify2 _ m = "(" ++ pretty m ++ ")"

pretty (MVal v) =
    case v of
      VInt n -> show n
      VLam (x :=> m) -> "λ" ++ x ++ "." ++ pretty m ++ ""
      VClo (x :=> m) env -> "(Λ" ++ x ++ "." ++ pretty m ++ ") {" ++ prettyEnv env ++ "}"

printExp = putStrLn . pretty

prettyEnv :: Env -> String
prettyEnv = (>>= prettyBinding) . M.toList
    where prettyBinding (var, val) = var ++ " -> " ++ pretty (MVal val) ++ "\n"
printEnv = putStrLn . prettyEnv

main = do
    exp <- readLn :: IO Program
    print . evaluate $ exp
