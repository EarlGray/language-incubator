{-# LANGUAGE DeriveFunctor, RankNTypes #-}
import Data.List as L

data Ty
  = TBool
  | TArr Ty Ty
  deriving (Show, Read, Eq)

type Var = String
type Ctx = [(Var, Ty)]
type Vars = [(Var, Expr)]

data ExprI inf
  = ETrue inf | EFalse inf
  | EIf (ExprI inf) (ExprI inf) (ExprI inf) inf
  | EVar Var inf
  | ELam (Var, Ty) (ExprI inf) inf
  | EApp (ExprI inf) (ExprI inf) inf
  deriving (Show, Read, Functor)

type TypedExpr = ExprI Ty
type Expr = ExprI ()

getty :: TypedExpr -> Ty
getty (ETrue ty) = ty
getty (EFalse ty) = ty
getty (EIf _ _ _ ty) = ty
getty (EVar _ ty) = ty
getty (ELam _ _ ty) = ty
getty (EApp _ _ ty) = ty

erase :: ExprI i -> Expr
erase = fmap (const ())

showtermty term ty = concat ["`", show term, ": ", show ty, "`"]

varIntro v vs = v : vs

as :: Expr -> Ty -> TypedExpr
exp `as` ty = fmap (const ty) exp

typechk :: Ctx -> Expr -> TypedExpr
typechk _ctx (ETrue _) = ETrue TBool
typechk _ctx (EFalse _) = EFalse TBool
typechk ctx tif@(EIf tcond tthen telse _) =
  let inf = getty . typechk ctx
      thenty = inf tthen
      elsety = inf telse
  in case getty (typechk ctx tcond) of
    TBool -> 
      if thenty == elsety
      then tif `as` thenty
      else error $ concat [showtermty tthen thenty,
                  " and ", showtermty telse elsety,
                  " must be of the same type"]
    ty -> error $ showtermty tcond ty ++ " must be TBool"
typechk ctx tv@(EVar var _) =
  case L.lookup var ctx of
    Just ty -> tv `as` ty
    Nothing -> error $ var ++ ": not found"
typechk ctx tlam@(ELam (var, ty) tbody _) =
  let ctx' = (var, ty) : ctx
  in tlam `as` TArr ty (getty $ typechk ctx' tbody)
typechk ctx lapp@(EApp t1 t2 _) =
  let ty1 = getty (typechk ctx t1)
      ty2 = getty (typechk ctx t2)
  in case (ty1, ty2) of
    (TArr ty1 ty2, ty1') | ty1 == ty1' -> lapp `as` ty2
    _ -> error $ concat [showtermty t1 ty1,
                    " ", showtermty t2 ty2,
                    " : invalid application"]

eval :: Vars -> Expr -> Expr
eval vars (EVar var _) = val
  where Just val = L.lookup var vars
eval _ lam@(ELam _ _ _) = lam
eval vars (EIf tcond tthen telse _) =
  case eval vars tcond of
    ETrue _ -> eval vars tthen
    EFalse _ -> eval vars telse
eval vars (EApp t1 t2 _) = eval (varIntro (arg, val) vars) tbody
  where
    ELam (arg, _) tbody _ = eval vars t1
    val = eval vars t2
eval _ b@(ETrue _) = b
eval _ b@(EFalse _) = b

{- DEBUG -}
true = ETrue ()
false = EFalse ()

var v = EVar v ()
app t1 t2 = EApp t1 t2 ()
lam (v, ty) body = ELam (v, ty) body ()

t_id = lam ("x", TBool) (var "x")

tcheck = getty . typechk []

test0 = tcheck (app (lam ("x", TBool) (var "x")) false) == TBool
