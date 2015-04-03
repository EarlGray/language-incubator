open Syntax
open Lexing
(** open Lexing *)

exception Fatal_error of string
exception SyntaxError of string

let fatal_error msg = raise (Fatal_error msg)
let syntax_error msg = raise (SyntaxError msg)

let lexer_from_channel fn ch =
  let lex = Lexing.from_channel ch in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = fn; pos_lnum = 1; } ;
    lex

let lexer_from_string str =
  let lex = Lexing.from_string str in
  let pos = lex.lex_curr_p in
    lex.lex_curr_p <- { pos with pos_fname = ""; pos_lnum = 1; } ;
    lex

let rec exec_cmd n (ctx, env) = function
  | Expr e ->
      let ty = Type_check.type_of ctx e in
      let v = Interpret.interp env e in
        print_string ("- : " ^ string_of_type ty ^ " = ") ;
        Interpret.print_result n v ;
        print_newline () ;
        (ctx, env)
  | Def (x, e) ->
      let ty = Type_check.type_of ctx e in
        print_endline ("val " ^ x ^ " : " ^ string_of_type ty) ;
        ((x,ty)::ctx, (x, ref (Interpret.VClosure (env, e)))::env)
  | Quit -> raise End_of_file
  | Use fn -> exec_file n (ctx, env) fn

and exec_file n ce fn =
  let fh = open_in fn in
  let lex = lexer_from_channel fn fh in
    try
      let cmds = Parser.toplevel Lexer.token lex in
        close_in fh ;
        exec_cmds n ce cmds
    with
      | Type_check.Type_error msg -> fatal_error (fn ^ ":\n" ^ msg)
      | Interpret.Runtime_error msg -> fatal_error ("Runtime error: " ^ msg)
      | Sys.Break -> fatal_error "Interrupted"
      | Parsing.Parse_error | Failure("lexing: empty token") ->
          fatal_error "Parse error"

and exec_cmds n ce cmds =
  List.fold_left (exec_cmd n) ce cmds
;;

let shell n ctx env =
  print_endline "Minihaskell" ;
  let osshortcut = (match Sys.os_type with
          "Unix" | "Cygwin" -> "Ctrl-D"
        | "Win32" -> "Ctrl-Z"
        | _ -> "EOF") in
    print_endline ("Press " ^ osshortcut ^ " to quit") ;
  let global_ctx = ref ctx in
  let global_env = ref env in
    try
      while true do
        try
          print_string "MiniHS> " ;
          let str = read_line ()  in
          let lex = lexer_from_string str in
          let cmds =
            try
              Parser.toplevel Lexer.token lex
            with
              | Failure("lexing: empty token")
              | Parsing.Parse_error -> fatal_error ("Parse: " ^ (Lexing.lexeme lex))
          in
          let (ctx, env) = exec_cmds n (!global_ctx, !global_env) cmds in
            global_ctx := ctx ;
            global_env := env
        with
          | Fatal_error msg -> print_endline ("Fatal error: " ^ msg)
          | Interpret.Runtime_error msg -> print_endline ("Runtime error: " ^ msg)
          | Type_check.Type_error msg -> print_endline ("Type error: " ^ msg)
          | Sys.Break -> print_endline "Interrupted"
      done
    with
      End_of_file -> print_endline "\nbye\n"


let main =
  Sys.catch_break true ;
  let print_depth = ref 100 in
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run the interactive shell");
       ("-p", Arg.Int (fun n -> print_depth := n), "set print depth")]
      (fun f -> files := f :: !files)
      "Usage: minihs [-p <int>] [-n] [file]..." ;
    files := List.rev !files ;
    let ctx, env =
      try List.fold_left (exec_file !print_depth) ([], []) !files
      with Fatal_error msg -> print_endline ("Fatal error: " ^ msg) ; exit 1
    in
      if not !noninteractive then
        shell !print_depth ctx env

let () = main
