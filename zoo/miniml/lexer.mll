{
  open Parser
}

let var = ['a'-'z' 'A'-'Z']

rule token = parse
  | [' ' '\t' '\n' '\r']  { token lexbuf }
  | ['0'-'9']             { INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "int"                 { TINT }
  | "bool"                { TBOOL }
  | "true"                { TRUE }
  | "false"               { FALSE }
  | "fun"                 { FUN }
  | "is"                  { IS }
  | "if"                  { IF }
  | "then"                { THEN }
  | "else"                { ELSE }
  | "let"                 { LET }
  | ";;"                  { SEMICOLON2 }
  | "="                   { EQUAL }
  | "<"                   { LESS }
  | "->"                  { TARROW }
  | ":"                   { COLON }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { TIMES }
  | var                   { VAR (Lexing.lexeme lexbuf) }
  | eof                   { EOF }

{
}
