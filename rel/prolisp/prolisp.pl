:- module(prolisp, []).

skip_spaces(Src) :-
  peek_char(Src, C),
  (char_type(C, space)
  ->  get_char(Src, C), skip_spaces(Src)
  ;   true).

not_symbol_char(C) :- memberchk(C, ['(', ')', '\'']).

parse_top(Src, Top) :- parse_top(Src, [], Top).
parse_top(Src, Forms, Top) :-
  parse_next(Src, Term),
  peek_char(Src, C),
  (C = end_of_file
  -> reverse([Term | Forms], Top)
  ; parse_top(Src, [Term | Forms], Top)).

parse_next(Src, Term) :-
  skip_spaces(Src),
  peek_char(Src, C), parse_starting_from(Src, C, Term),
  skip_spaces(Src).
parse_starting_from(Src, '(', Term) :- get_char(Src, '('), parse_form(Src, Term).
parse_starting_from(Src, _, Term) :- parse_id(Src, Term).

parse_form(Src, Form) :- parse_form(Src, [], Form).
parse_form(Src, Terms, Form) :-
  parse_next(Src, Term),
  peek_char(Src, C),
  (C == ')'
  -> get_char(Src, ')'), reverse([Term | Terms], Form)
  ; parse_form(Src, [Term | Terms], Form)).

parse_id(Src, Term) :- parse_id(Src, [], Term).
parse_id(Src, Cs, Term) :-
  peek_char(Src, C),
  (not_symbol_char(C)
  -> reverse(Cs, RCs), atom_chars(Term, RCs)
  ; (char_type(C, space)
    -> reverse(Cs, RCs), atom_chars(Term, RCs)
    ; get_char(Src, C), parse_id(Src, [C|Cs], Term))).

parse_file(FileName, Forms) :-
  open(FileName, read, Src),
  parse_top(Src, [], Forms),
  write(Forms),
  close(Src).

eval(_, _, [], []) :- !.
eval(_, _, T, T) :- number(T), !.
eval(_, _, T, Num) :- atom(T), atom_number(T, Num), !.

eval(Store, Ctx, T, V) :- atom(T), !,
  (memberchk({T, V}, Ctx); memberchk({T, V}, Store)).

eval(Store, Ctx, [Form | Args], Res) :-
  eval(Store, Ctx, Form, F), % TODO: eval args
  fapply(Store, Ctx, F, Args, Res).

fadd(_, _, X, Y, Z) :- Z is X + Y.
fmul(_, _, X, Y, Z) :- Z is X * Y.

fapply(Store, Ctx, prolog(Fun, 1), [Arg1], Res) :-
  apply(Fun, [Store, Ctx, Arg1, Res]).
fapply(Store, Ctx, prolog(Fun, 2), [Arg1, Arg2], Res) :-
  apply(Fun, [Store, Ctx, Arg1, Arg2, Res]).
fapply(Store, Ctx, prolog(Fun, 3), [Arg1, Arg2, Arg3], Res) :-
  apply(Fun, [Store, Ctx, Arg1, Arg2, Arg3, Res]).

% vim: set syntax=prolog ts=2 sw=2
