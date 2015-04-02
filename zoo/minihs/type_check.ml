(***********************************************
    Taken from
 http://andrej.com/plzoo/html/minihaskell.html 
************************************************)

open Syntax

exception Type_error of string

let type_error msg = raise (Type_error msg)

let rec check ctx ty e =
  let ty' = type_of ctx e in
    if ty' <> ty then
      type_error (string_of_expr e ^ " has type " ^ string_of_type ty' ^
                  " but is used as if it has type " ^ string_of_type ty)

and type_of ctx = function
  | Var x -> (try List.assoc x ctx with Not_found -> type_error ("unknown identifier: " ^ x))
  | Int _ -> TInt
  | Bool _ -> TBool
  | Nil ty -> TList ty
  | Times (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Divide (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Mod (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Plus (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Minus (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TInt
  | Equal (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
  | Less (e1, e2) -> check ctx TInt e1 ; check ctx TInt e2 ; TBool
  | Cons (e1, e2) -> let ty = type_of ctx e1 in check ctx (TList ty) e2 ; TList ty
  | If (e1, e2, e3) ->
      check ctx e1 TBool ;
      let ty = type_of ctx e2 in
        check ctx ty e3 ;
      ty
  | Fun (x, ty, e) -> TArrow (ty, type_of ((x, ty)::ctx) e)
  | Rec (x, ty, e) -> check ((x, ty)::ctx) ty e ; ty
  | Match (e1, ty, e2, x, y, e3) ->
      (match type_of ctx e1 with
        | TList ty1 ->
            check ctx (TList ty) e1 ;
            let ty2 = type_of ctx e2 in
              check ((x, ty)::(y, TList ty)::ctx) ty2 e3 ;
            ty2
        | ty -> type_error (string_of_expr e1
                           ^ " is used as a list but its type is "
                           ^ string_of_type ty))
  | Apply (e1, e2) ->
    (match type_of ctx e1 with
      | TArrow (ty1, ty2) -> check ctx ty1 e2 ; ty2 
      | ty -> type_error (string_of_expr e1
                         ^ " is used as a function but its type is "
                         ^ string_of_type ty))
  | Pair (e1, e2) -> TTimes (type_of ctx e1, type_of ctx e2)
  | Fst e ->
    (match type_of ctx e1 with 
      | TTimes (ty1, _) -> ty1
      | ty -> type_error (string_of_expr e
                         ^ " is used a pair, but its type is "
                         ^ string_of_type ty))
  | Snd e ->
    (match type_of ctx e1 with 
      | TTimes (_, ty) -> ty
      | ty -> type_error (string_of_expr e
                         ^ " is used a pair, but its type is "
                         ^ string_of_type ty))

