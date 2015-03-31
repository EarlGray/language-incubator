open Syntax
open Machine

let rec compile = function
  | Var x   -> [IVar x]
  | Int k   -> [IInt k]
  | Bool b  -> [IBool b]
  | Times (e1, e2)  -> (compile e1) @ (compile e2) @ [IMult]
  | Plus  (e1, e2)  -> (compile e1) @ (compile e2) @ [IAdd]
  | Minus (e1, e2)  -> (compile e1) @ (compile e2) @ [ISub]
  | Equal (e1, e2)  -> (compile e1) @ (compile e2) @ [IEqual]
  | Less (e1, e2)   -> (compile e1) @ (compile e2) @ [ILess]
  | If (e1, e2, e3) -> (compile e1) @ [IBranch (compile e2, compile e2)]
  | Fun (f, x, _, _, e) -> [IClosure (f, x, compile e @ [IPopEnv])]
  | Apply (e1, e2)    -> (compile e1) @ (compile e2) @ [ICall]
