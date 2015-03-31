(******************************************

http://andrej.com/plzoo/html/miniml.html

*******************************************)

type name = string

type ty =
    | TInt
    | TBool
    | TArrow of ty * ty

type expr = 
    | Var of name
    | Int of int
    | Bool of bool
    | Times of expr * expr
    | Plus of expr * expr
    | Minus of expr * expr
    | Equal of expr * expr
    | Less of expr * expr
    | If of expr * expr * expr
    | Fun of name * name * ty * ty * expr
    | Apply of expr * expr

type toplevel_cmd =
    | Expr of expr
    | Def of name * expr

let string_of_type ty = 
  let rec to_str n ty =
    let (m, str) =
      match ty with
      | TInt -> (2, "int")
      | TBool -> (2, "bool")
      | TArrow (ty1, ty2) -> (1, (to_str 1 ty1) ^ " -> " ^ (to_str 0 ty2))
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) ty

let string_of_expr e =
  let rec to_str n e =
    let (m, str) =
      match e with
      | Int n           -> (7, string_of_int n)
      | Bool b          -> (7, string_of_bool b)
      | Var x           -> (7, x)
      | Apply (e1, e2)  -> (6, (to_str 5 e1) ^ " " ^ (to_str 6 e2))
      | Times (e1, e2)  -> (5, (to_str 4 e1) ^ " * " ^ (to_str 5 e2))
      | Plus (e1, e2)   -> (4, (to_str 3 e1) ^ " + " ^ (to_str 4 e2))
      | Minus (e1, e2)  -> (4, (to_str 3 e1) ^ " - " ^ (to_str 4 e2))
      | Equal (e1, e2)  -> (3, (to_str 3 e1) ^ " = " ^ (to_str 3 e2))
      | Less (e1, e2)   -> (3, (to_str 3 e1) ^ " = " ^ (to_str 3 e2))
      | If (e1, e2, e3) -> (2, "if " ^ (to_str 2 e1) ^ 
                               " then " ^ (to_str 2 e2) ^ 
                               " else " ^ (to_str 2 e3))
      | Fun (f, x, ty1, ty2, e) ->
            let dfn  = "fun " ^ f ^ "(" ^ x ^ ": " ^ 
                        (string_of_type ty1) ^ ") : " ^ (string_of_type ty2) in
            let body = "{ " ^ (to_str 0 e) ^ " }" in
            (1, dfn ^ " " ^ body)
    in
      if m > n then str else "(" ^ str ^ ")"
  in
    to_str (-1) e


let rec subst s = function
  | (Var x) as e            -> (try List.assoc x s with Not_found -> e)
  | (Int _ | Bool _) as e   -> e
  | Times (e1, e2)  -> Times (subst s e1, subst s e2)
  | Plus (e1, e2)   -> Plus (subst s e1, subst s e2)
  | Minus (e1, e2)  -> Minus (subst s e1, subst s e2)
  | Equal (e1, e2)  -> Equal (subst s e1, subst s e2)
  | Less (e1, e2)   -> Less (subst s e1, subst s e2)
  | If (e1, e2, e3) -> If (subst s e1, subst s e2, subst s e3)
  | Fun (f, x, ty1, ty2, e) ->
        let s' = List.remove_assoc f (List.remove_assoc x s) in
          Fun (f, x, ty1, ty2, subst s' e)
  | Apply (e1, e2)  -> Apply (subst s e1, subst s e2)

