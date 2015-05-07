open Syntax

type choice = database * environment * clause * int

(* global database of assertions *)
let base = ref ([] : database)

(* add a new asserion at the end of the current database *)
let assertz a =
  let rec add = function [] -> [a] | b::bs -> b::(add bs) in
    (base := add !base)

(* goal can't be proved *)
exception NoSolution

let rec renumber_term n = function
  | Var (x,_) -> Var (x,n)
  | Const _ as c -> c
  | App (c, ts) -> App (c, List.map (renumber_term n) ts)

let rec renumber_atom n (c,ts) = (c, List.map (renumber_term n) ts)

let rec display_solution ch env =
  match string_of_env env, ch with
  | "Yes", _ -> print_endline "Yes"
  | answer, [] -> print_endline answer
  | answer, ch -> begin
      print_string (answer ^ "\nmore? (y/n) [y] ") ;
      flush stdout ;
      match String.lowercase (read_line ()) with
        | "y" | "yes" | "" -> continue_search ch
        | _ -> raise NoSolution
    end

and continue_search = function
  | [] -> raise NoSolution
  | (asrl,env,gs,n)::cs -> solve cs asrl env gs n
 
and solve ch asrl env c n =
  let rec reduce_atom a = function
    | [] -> None
    | (b,lst)::asrl' ->
      (try let env' = Unify.unify_atoms env a (renumber_atom n b) in
            Some (asrl', env', List.map (renumber_atom n) lst)
       with Unify.NoUnify -> reduce_atom a asrl')
  in
    match c with
    | [] -> display_solution ch env
    | a::c' ->
      (match reduce_atom a asrl with
        | None -> continue_search ch    (* this clause can't be solved *)
        | Some (asrl', env', d) ->      (* this atom reduced to subgoals *)
            let ch' = (asrl', env, c, n)::ch  (* add a new choice *)
            in solve ch' !base env' (d @ c') (n + 1))

let solve_toplevel c = 
  try solve [] !base [] c 1
  with NoSolution -> print_endline "No"

            
