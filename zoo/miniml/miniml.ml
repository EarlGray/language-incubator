(******************************************

http://andrej.com/plzoo/html/miniml.html

*******************************************)

open Syntax

type context = (name * ty) list

type env = (name * Machine.mvalue) list

let exec_cmd (ctx, env) = function
  | Expr e -> (* typecheck, compile and run expression *)
      let ty = Type_check.type_of ctx e in
      let frm = Compile.compile e in 
      let v = Machine.run frm env in
        ((ctx, env),
         "- : " ^ (Syntax.string_of_type ty) ^ " = " ^ (Machine.string_of_mvalue v))
  | Def (x, e) -> (* typecheck [e], compile it, run it 
                    and return a new context-environment pair with [x] defined as [e] *)
        let ty = Type_check.type_of ctx e in
        let frm = Compile.compile e in
        let v = Machine.run frm env in
          (((x, ty)::ctx, (x,v)::env),
           x ^ " : " ^ (Syntax.string_of_type ty) ^ " = " ^ (Machine.string_of_mvalue v))
;;

let exec_cmds ce cmds = 
  List.fold_left
    (fun ce cmd -> let (ce', msg) = exec_cmd ce cmd in print_endline msg ; ce')
    ce cmds
;;

let shell ctx env =
  print_endline "=========== MiniML ==============";
  let osshct = (match Sys.os_type with 
                  "Unix"|"Cygwin" -> "Ctrl-D"
                | "Win32" -> "Ctrl-Z"
                | _ -> "EOF" ) in
    print_endline ("Press " ^ osshct ^ " to exit");
  let global_ctx = ref ctx in
  let global_env = ref env in
    try
      while true do
        try 
          (* read, parse, execute *)
          print_string "MiniML> ";
          let str = read_line () in
          let cmds = Parser.toplevel Lexer.token (Lexing.from_string str) in
          let (ctx, env) = exec_cmds (!global_ctx, !global_env) cmds in
            global_ctx := ctx ;
            global_env := env
        with
          | Type_check.Type_error msg -> print_endline ("Type error: " ^ msg)
          | Machine.Machine_error msg -> print_endline ("Machine error: " ^ msg)
          | Failure _ | Parsing.Parse_error -> print_endline ("Syntax error")
      done
    with
      End_of_file -> print_endline "bye" 

let main =
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run the interactive shell")]
       (fun f -> files := f :: !files)
       "Usage: miniml [-n] [file1]..." ;
    try
      let ctx, env =
        List.fold_left
          (fun ce f ->
            let fh = open_in f in
            let cmds = Parser.toplevel Lexer.token (Lexing.from_channel fh) in
              close_in fh;
              exec_cmds ce cmds)
          ([], []) !files
      in
        if not !noninteractive then shell ctx env
    with
      | Type_check.Type_error msg -> print_endline ("Type error: " ^ msg)
      | Machine.Machine_error msg -> print_endline ("Machine error: " ^ msg)
      | Failure _ | Parsing.Parse_error -> print_endline "Syntax error"

let () = main
