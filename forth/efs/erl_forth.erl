-module(erl_forth).
-compile(export_all).

parse_with(Src, Parser) ->
  {ok, Tokens, _} = erl_scan:string(Src),
  {ok, AST} = Parser(Tokens),
  AST.

parse_form(Src) -> parse_with(Src, fun erl_parse:parse_form/1).
parse_expr(Src) -> parse_with(Src, fun erl_parse:parse_exprs/1).


