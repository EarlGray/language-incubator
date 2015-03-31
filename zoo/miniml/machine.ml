type name = Syntax.name

type mvalue =
  | MInt of int
  | MBool of bool
  | MClosure of name * frame * environ

and instr = 
  | IMult
  | IAdd
  | ISub
  | IEqual
  | ILess
  | IVar of name
  | IInt of int
  | IBool of bool
  | IClosure of name * name * frame
  | IBranch of frame * frame
  | ICall 
  | IPopEnv

and frame = instr list

and environ = (name * mvalue) list

and stack = mvalue list

exception Machine_error of string

let error msg = raise (Machine_error msg)

let string_of_mvalue = function
  | MInt k  -> string_of_int k
  | MBool b -> string_of_bool b
  | MClosure _ -> "<fun>"

let lookup x = function
    env::_  -> (try List.assoc x env with Not_found -> error ("unknown " ^ x))
  | _       -> error ("unknown " ^ x)

let pop = function
  | []    -> error "stack underflow"
  | v::vs -> (v, vs)

let pop_bool = function
  | MBool b :: s  -> (b, s)
  | _             -> error "bool expected"

let pop_app = function
  | v :: MClosure (x, f, e) :: s -> (x, f, e, v, s)
  | _       -> error "value and closure expected"

let mult = function
  | (MInt x) :: (MInt y) :: s -> MInt (x * y) :: s
  | _ -> error "int and int expected in add"

let add = function
  | (MInt x) :: (MInt y) :: s -> MInt (x + y) :: s
  | _ -> error "int and int expected in mult"

let sub = function
  | (MInt x) :: (MInt y) :: s -> MInt (y - x) :: s
  | _   -> error "int and int expected in sub"

let equal = function
  | (MInt x) :: (MInt y) :: s -> MBool (y = x) :: s
  | _   -> error "int and int expected in equal"

let less = function
  | (MInt x) :: (MInt y) :: s -> MBool (y < x) :: s
  | _   -> error "int and int expected in less"

let exec instr frms stck envs =
  match instr with
  | IMult   -> (frms, mult stck, envs)
  | IAdd    -> (frms, add stck, envs)
  | ISub    -> (frms, sub stck, envs)
  | IEqual  -> (frms, equal stck, envs)
  | ILess   -> (frms, less stck, envs)
  | IInt k  -> (frms, (MInt k) :: stck, envs)
  | IBool b -> (frms, (MBool b) :: stck, envs)
  | IVar v  -> (frms, (lookup v envs) :: stck, envs)
  | IClosure (f, x, frm) ->
      (match envs with
      | env :: _ ->
          let rec c = MClosure (x, frm, (f, c):: env) in
            (frms, c :: stck, envs)
      | [] -> error "no environment for a closure")
  | IBranch (f1, f2) ->
      let (b, stck') = pop_bool stck in
        ((if b then f1 else f2) :: frms, stck', envs)
  | ICall ->
      let (x, frm, env, v, stck') = pop_app stck in
        (frm :: frms, stck', ((x, v) :: env) :: envs)
  | IPopEnv ->
      match envs with
      | []  ->  error "no environment to pop"
      | _ :: envs' -> (frms, stck, envs')

 
let run frm env =
  let rec loop = function
    | ([], [v], _)  -> v
    | ((i::is) :: frms, stck, envs) ->  loop (exec i (is::frms) stck envs)
    | ([] :: frms, stck, envs)  -> loop (frms, stck, envs)
    | _     -> error "illegal end of program"

  in
    loop ([frm], [], [env])


