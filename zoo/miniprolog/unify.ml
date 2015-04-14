open Syntax

exception NoUnify

(* returns environment extended by unification results *)
let rec unify_terms env t1 t2 =
  match subst_term env t1, subst_term env t2 with
  | t1, t2 when t1 = t2 -> env
  | (Var y, t) | (t, Var y) ->
    if occurs y t then
      raise NoUnify
    else
      (y, t) :: env
  | Const _, _ -> raise NoUnify
  | App (c1, ts1), App (c2, ts2) when c1 = c2 -> unify_lists env ts1 ts2
  | App _, _ -> raise NoUnify

and unify_lists env lst1 lst2 =
  try
    List.fold_left2 (fun env t1 t2 -> unify_terms env t1 t2) env lst1 lst2
  with Invalid_argument _ -> raise NoUnify

let unify_atoms env (c1, ts1) (c2, ts2) =
  if c1 = c2 then unify_lists env ts1 ts2 else raise NoUnify
