-- see: http://www.andres-loeh.de/LambdaPi/LambdaPi.pdf
import Control.Monad

data TermInf
  = Ann TermChk Type
  | Bound Int
  | Free Name
  | TermInf :@: TermChk
  deriving (Show, Eq)

data TermChk
  = Inf TermInf
  | Lam TermChk
  deriving (Show, Eq)

data Name
  = Global String
  | Local Int
  | Quote Int
  deriving (Show, Eq)

data Type
  = TFree Name
  | Fun Type Type
  deriving (Show, Eq)

data Value
  = VLam (Value -> Value)
  | VNeutral Neutral

data Neutral
  = NFree Name
  | NApp Neutral Value

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

vapp :: Value -> Value -> Value
vapp (VLam f)     v = f v
vapp (VNeutral n) v = VNeutral (NApp n v)

type Env = [Value]

evalinf :: TermInf -> Env -> Value
evalinf (Ann e _) d = evalchk e d
evalinf (Free x)  d = vfree x
evalinf (Bound i) d = d !! i
evalinf (e :@: e') d = vapp (evalinf e d) (evalchk e' d)

evalchk :: TermChk -> Env -> Value
evalchk (Inf i) d = evalinf i d
evalchk (Lam e) d = VLam (\x -> evalchk e (x : d))

data Kind = Star
  deriving (Show)

data Info
  = HasKind Kind
  | HasType Type
  deriving (Show)

type Context = [(Name, Info)]

type Result a = Either String a
throwError s = Left s

kindchk :: Context -> Type -> Kind -> Result ()
kindchk gamma (TFree x) Star
  = case lookup x gamma of
      Just (HasKind Star) -> return ()
      Nothing             -> throwError $ "unknown identifier" ++ show x
kindchk gamma (Fun k k') Star = 
   kindchk gamma k Star >> kindchk gamma k' Star

typeinf0 :: Context -> TermInf -> Result Type
typeinf0 = typeinf 0

typeinf :: Int -> Context -> TermInf -> Result Type
typeinf i gamma (Ann e t) = do
    kindchk gamma t Star
    typechk i gamma e t
    return t
typeinf i gamma (Free x) =
  case lookup x gamma of 
    Just (HasType t) -> return t
    Nothing -> throwError $ "unknown identifier " ++ show x
typeinf i gamma (e :@: e') = do
  sigma <- typeinf i gamma e
  case sigma of
    Fun t t' -> typechk i gamma e' t >> return t'
    _ -> throwError "illegal application"

typechk :: Int -> Context -> TermChk -> Type -> Result ()
typechk i gamma (Inf e) t = do
  t' <- typeinf i gamma e
  unless (t == t') $ throwError "type mismatch"
typechk i gamma (Lam e) (Fun t t') =
  typechk (i + 1) ((Local i, HasType t) : gamma) (substchk 0 (Free (Local i)) e) t'
typechk i gamma _ _ =
  throwError "type mismatch"

substinf :: Int -> TermInf -> TermInf -> TermInf
substinf i r (Ann e t) = Ann (substchk i r e) t
substinf i r (Bound j) = if i == j then r else Bound j
substinf i r (Free y)  = Free y
substinf i r (e :@: e') = substinf i r e :@: substchk i r e'

substchk :: Int -> TermInf -> TermChk -> TermChk
substchk i r (Inf e) = Inf (substinf i r e)
substchk i r (Lam e) = Lam (substchk (i+1) r e)

quote0 :: Value -> TermChk
quote0 = quote 0

quote :: Int -> Value -> TermChk
quote i (VLam f)     = Lam (quote (i + 1) (f (vfree (Quote i))))
quote i (VNeutral n) = Inf (neutralQuote i n)

neutralQuote :: Int -> Neutral -> TermInf
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = neutralQuote i n :@: quote i v

boundfree :: Int -> Name -> TermInf
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i x         = Free x


{-
 -    Examples
 -}

id'     = Lam (Inf (Bound 0))
const'  = Lam (Lam (Inf (Bound 1)))

tfree a = TFree (Global a)
free x  = Inf (Free (Global x))

term1   = Ann id' (Fun (tfree "a") (tfree "a")) :@: free "y"
term2   = Ann const' (Fun (Fun (tfree "b") (tfree "b"))
                          (Fun (tfree "a")
                               (Fun (tfree "b") (tfree "b"))))
          :@: id' :@: free "y"

env1    = [(Global "y", HasType (tfree "a")),
           (Global "a", HasKind Star)]
env2    = [(Global "b", HasKind Star)] ++ env1
