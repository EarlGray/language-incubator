:- begin_tests(fullsimple).
:- use_module(fullsimple).

test(t_unit, [nondet]) :-
  fullsimple:type([], unit, unit).

test(t_ite) :-
  fullsimple:type([], ite(true, 1, 0), int).
test(t_ite_fail, [fail]) :-
  fullsimple:type([], ite(bool, 2, z), int).

:- end_tests(fullsimple).

% vim: syntax=prolog
