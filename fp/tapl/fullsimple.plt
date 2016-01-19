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

test(t_case_ret) :-
  fullsimple:type([], case(inl(true), {l, 1}, {r, 0}), int), !.

test(t_inr0) :- fullsimple:type([], inr(4), uni(_, int)), !.
test(t_inl0) :- fullsimple:type([], inl(true), uni(bool, _)), !.

test(t_case_let) :-
  Term = let({u, inr(4)}, case(u, {l, l}, {r, plus(r, r)})),
  fullsimple:type([], Term, int), !.

test(e_case_let0) :-
  Term = let({u, inr(4)}, case(u, {l, l}, {r, plus(r, r)})),
  fullsimple:eval([], Term, 8), !.

test(e_case_ite0) :-
  Term = case(inr(true), {l, 1}, {r, ite(r, 2, 3)}),
  fullsimple:eval([], Term, 2), !.

:- end_tests(fullsimple).

% vim: syntax=prolog
