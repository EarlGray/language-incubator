:- begin_tests(fullref).
:- ensure_loaded(fullref).

test(t_unit) :- fullref:type([], [], unit, Ty), Ty = unit.
test(t_var_ok) :- fullref:type([], [{x, int}], x, Ty), Ty = int.
test(t_var_unknown) :- fullref:type([], [{x, int}], y, Ty), Ty = err({unknown_var, y}).

test(t_ref_unit) :- fullref:type([], [], ref(unit), Ty), Ty = ref(unit).
test(t_ref_ctx) :- fullref:type([], [{x, unit}], ref(x), Ty), Ty = ref(unit).

test(t_dereftwice_ctx) :- fullref:type([], [{x, ref(ref(unit))}], @(@(x)), Ty), Ty = unit.
test(t_deref_fail) :-
  fullref:type([], [{x, ref(unit)}], @(@(x)), Ty),
  Ty = err({'T-Deref', unit, @(x)}).
test(t_set_0) :- fullref:type([], [{x, ref(unit)}], set(x, unit), Ty), Ty = unit.
test(t_set_typemismatch) :-
  fullref:type([], [{x, ref(lam(v, v))}], set(x, unit), Ty),
  Ty = err({'T-Assign, type mismatch', lam(X, X), unit}).
test(t_set_refexpected) :-
  fullref:type([], [{x, unit}], set(x, unit), Ty),
  Ty = err({'T-Assign, ref expected:', x}). 

test(t_lam_id) :- fullref:type([], [], lam(x, x), Ty), Ty = arr(Any, Any).

:- end_tests(fullref).

% vim: syntax=prolog ts=2 sw=2
