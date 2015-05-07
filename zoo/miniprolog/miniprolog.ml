open Message
open Syntax

exception Fatal_error of string

let fatal_error msg = raise (Fatal_error msg)

let rec exec_cmd = function
  | Goal g -> Solve.solve_toplevel g
  | Assert a -> Solve.assertz a
  | Quit -> raise End_of_file
  | Use fn -> exec_file fn

and exec_file fn =
  let fh = open_in fn in
  let lex = Message.lexer_from_channel fn fh in
    try
      let cmds = Parser.toplevel Lexer.token lex in
        close_in fh ;
        exec_cmds cmds
    with
      | Sys.Break -> fatal_error "Interrupted."
      | Parsing.Parse_error | Failure("lexing: empty token") -> 
          fatal_error (Message.syntax_error lex)

and exec_cmds cmds = List.iter exec_cmd cmds
;;

let shell () =
  let osshortcut =
    (match Sys.os_type with
      | "Unix" | "Cygwin" -> "Ctrl-D"
      | "Win32" -> "Ctrl-Z"
      | _ -> "EOF") in
  print_endline "Miniprolog." ;
  print_endline ("Press " ^ osshortcut ^ " to exit") ;
  print_endline "Input syntax: " ;
  print_endline "   ?- query.          %% make a query" ;
  print_endline "   a(t1, ..., tN).    %% Assert an atomic proposition" ;
  print_endline "   A :- B1, ..., Bn.  %% Assert an inference rule" ;
  print_endline "   $quit              %% exit the interpreter" ;
  print_endline "   $use \"filename\"    %% execute commands from a file" ;
  try 
    while true do
      try
        print_string "prolog> " ;
        let str = read_line () in
        let lex = Message.lexer_from_string str in
        let cmds =
          try
            Parser.toplevel Lexer.token lex
          with
            | Failure ("lexing: empty token")
            | Parsing.Parse_error -> fatal_error (Message.syntax_error lex)
        in
          exec_cmds cmds
      with
        | Fatal_error msg -> Message.report msg
        | Sys.Break -> Message.report ("Interrupted.")
    done
  with
    End_of_file -> print_endline "\nGood bye."

let main =
  Sys.catch_break true ;
  let noninteractive = ref false in
  let files = ref [] in
    Arg.parse
      [("-n", Arg.Set noninteractive, "do not run the interactive shell")]
      (fun f -> files := f :: !files)
      "Usage: miniprolog [-n] [file]..." ;
    files := List.rev !files ;
    try
      List.iter exec_file !files ;
      if not !noninteractive then shell ()
    with Fatal_error msg -> Message.report msg
      
 
let () = main 
