:- begin_tests(fullsimple).
:- ensure_loaded(fullsimple).

test(t_unit, [nondet]) :-
  fullsimple:type([], unit, unit).

test(t_ite, [nondet]) :-
  fullsimple:type([], ite(true, 1, 0), int).
test(t_ite_fail, [fail]) :-
  fullsimple:type([], ite(bool, 2, z), int).

test(t_appid, [nondet]) :-
  Term = app(lam(x, x), lam(x, x)),
  fullsimple:type([], Term, arr(X, X)).

test(t_plus0) :-
  Term = plus(2, 2),
  fullsimple:type([], Term, int), !.

test(e_plus0) :-
  Term = plus(2, 2),
  fullsimple:eval([], Term, 4), !.

test(e_ctx) :-
  fullsimple:eval([{x, 42}], x, 42), !.

test(e_let0) :-
  fullsimple:eval([], let({x, 42}, x), 42), !.

:- end_tests(fullsimple).

% vim: syntax=prolog
