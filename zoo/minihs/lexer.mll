(***********************************************
    Taken from
 http://andrej.com/plzoo/html/minihaskell.html 
************************************************)


{
  open Parser
  open Lexing

  let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

}

let var = ['_' 'a'-'z' 'A'-'Z'] ['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule token = parse
  | '$' [^'\n']* '\n'   { incr_linenum lexbuf ; token lexbuf }
  | '\n'                { incr_linenum lexbuf ; token lexbuf }
  | [' ' '\t']          { token lexbuf }
  | ['0' '9']+          { INT (int_of_string(lexeme lexbuf)) }
  | "bool"              { TBOOL }
  | "else"              { ELSE }
  | "false"             { FALSE }
  | "fst"               { FST }
  | "fun"               { FUN }
  | "if"                { IF }
  | "int"               { TINT }
  | "is"                { IS }
  | "let"               { LET }
  | "list"              { TLIST }
  | "match"             { MATCH }
  | "rec"               { REC }
  | "snd"               { SND } 
  | "then"              { THEN }
  | "true"              { TRUE }
  | "$use"              { USE }
  | "$quit"             { QUIT }
  | "with"              { WITH }
  | "->"                { ARROW }
  | "::"                { CONS }
  | ";;"                { SEMICOLON2 }
  | '\"' [^'\"']* '\"'  { let str = lexeme lexbuf in
                          STRING (String.sub str 1 (String.length str - 2)) }
  | '%'                 { MOD }
  | '('                 { LPAREN }
  | ')'                 { RPAREN }
  | '['                 { LBRACK }
  | ']'                 { RBRACK }
  | '|'                 { ALTERNATIVE }
  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIVIDE }
  | ','                 { COMMA }
  | ':'                 { COLON }
  | '='                 { EQUAL }
  | '<'                 { LESS }
  | var                 { VAR (lexeme lexbuf) }
  | eof                 { EOF }

{
}
  
