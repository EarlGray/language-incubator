-- see: http://docs.idris-lang.org/en/latest/tutorial/interp.html
-- module Interp

import Data.Vect
import Data.Fin

data Ty = TyInt | TyBool | TyFun Ty Ty

interpTy : Ty -> Type
interpTy TyInt       = Int
interpTy TyBool      = Bool
interpTy (TyFun A T) = interpTy A -> interpTy T

using (G:Vect n Ty)  -- the context
  -- `HasType i G T` is a proof that variable `i` in context `G` has type `T`
  data HasType : (i : Fin n) -> Vect n Ty -> Ty -> Type where
    -- proof that the most recently defined variable is well-typed:
    Stop : HasType FZ (t :: G) t                       -- de-Brujin index 0
    Pop  : HasType k G t -> HasType (FS k) (u :: G) t  -- Pop Stop is index 1, ...

  data Expr : Vect n Ty -> Ty -> Type where
    Var : HasType i G t 
          -> Expr G t
    Val : (x : Int)
          -> Expr G TyInt
    Lam : Expr (a :: G) t
          -> Expr G (TyFun a t)
    App : Expr G (TyFun a t)
          -> Expr G a
          -> Expr G t
    Op  : (interpTy a -> interpTy b -> interpTy c) ->
          Expr G a -> Expr G b
          -> Expr G c
    If  : Expr G TyBool -> Lazy (Expr G a) -> Lazy (Expr G a)
          -> Expr G a

  data Env : Vect n Ty -> Type where
    -- given a proof that a variable if defined in the context,
    --   we can produce a value from the environment:
    Nil   : Env Nil
    (::)  : interpTy a -> Env G -> Env (a :: G)
  
  lookup : HasType i G t -> Env G -> interpTy t
  lookup Stop    (x :: xs) = x
  lookup (Pop k) (x :: xs) = lookup k xs

  interp : Env G -> Expr G t -> interpTy t
  interp env (Var i)      = lookup i env
  interp env (Val x)      = x
  interp env (Lam sc)     = \x => interp (x :: env) sc
  interp env (App f s)    = (interp env f) (interp env s)
  interp env (Op op x y)  = op (interp env x) (interp env y)
  interp env (If cnd thn els)
                          = if interp env cnd
                            then interp env thn
                            else interp env els

  succ : Expr G (TyFun TyInt TyInt)
  succ = Lam (Op (+) (Var Stop) (Val 1))

  add : Expr G (TyFun TyInt (TyFun TyInt TyInt)) -- \x.\y.y + x
  add = Lam (Lam (Op (+) (Var Stop) (Var (Pop Stop))))
  -- interp [] (App (App add (Val 2)) (Val 2)) gives 4 : Int

  iszero : Expr G (TyFun TyInt TyBool)
  iszero = Lam (Op (==) (Var Stop) (Val 0))

  -- interp [] (App fact (Val 4)) gives 24 : Int
  fact : Expr G (TyFun TyInt TyInt)
  fact = Lam (If (App iszero (Var Stop))
                 (Val 1)
                 (Op (*) (Var Stop)
                         (App fact (Op (-) (Var Stop) (Val 1)))))

main : IO ()
main = do
  putStr "Enter a number: "
  n <- getLine
  printLn (interp [] fact (cast n))
