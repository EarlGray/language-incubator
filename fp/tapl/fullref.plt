:- begin_tests(fullref).
:- ensure_loaded(fullref).

test(t_unit) :- fullref:type([], [], unit, Ty), Ty = unit.
test(t_var_ok) :- fullref:type([], [{x, int}], x, Ty), Ty = int.
test(t_var_unknown) :- fullref:type([], [{x, int}], y, Ty), Ty = err({unknown_var, y}).

test(t_ref_unit) :- fullref:type([], [], ref(unit), Ty), Ty = ref(unit).
test(t_ref_num)  :- fullref:type([], [], ref(5), Ty), Ty = ref(int).
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

test(t_seq_nothing) :- fullref:type([], [], do([]), Ty), Ty = err(_).
test(t_seq_one)     :- fullref:type([], [{y, ref(int)}, {x, int}], do([x]), Ty), Ty = int.
test(t_seq_twice)   :-
  Term = do([set(y, x), @(y)]),
  fullref:type([], [{y, ref(int)}, {x, int}], Term, Ty), Ty = int.

test(t_lam_id) :- fullref:type([], [], lam(x, x), Ty), Ty = arr(Any, Any).

% Store tests
test(st_new_get) :- fullref:store(H0),
  fullref:store_new(H0, H1, Loc, 5),
  fullref:store_get(H1, Loc, V1), V1 = 5.
test(st_new_set) :- fullref:store(H0),
  fullref:store_new(H0, H1, Loc, 5), fullref:store_get(H1, Loc, 5),
  fullref:store_set(H1, H2, Loc, 6), fullref:store_get(H2, Loc, 6).

% Eval tests
test(e_unit) :- fullref:eval([], {H, unit}, {H, unit}).
test(e_int)  :- fullref:eval([], {H, 5}, {H, 5}).
test(e_var)  :- fullref:eval([{x, 5}], {H, x}, {H, 5}).
test(e_app)  :- fullref:eval([{id, lam(x, x)}], {H, app(id, 5)}, {H, 5}).

test(e_let)  :- fullref:store(H0),
  fullref:eval([], {H0, let(r=ref(2), @(r))}, {_H1, 2}).
test(e_do_set_deref) :- fullref:store(H0),
  fullref:eval([], {H0, let(r=ref(0), do([set(r, 1), @(r)]))}, {_H1, 1}).

:- end_tests(fullref).

% vim: syntax=prolog ts=2 sw=2
