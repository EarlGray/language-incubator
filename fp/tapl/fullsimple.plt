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
  fullsimple:type([], Term, int).

test(e_plus0) :-
  Term = plus(2, 2),
  fullsimple:eval([], Term, 4).

test(e_ctx) :-
  fullsimple:eval([{x, 42}], x, 42).

test(e_let0) :-
  fullsimple:eval([], let({x, 42}, x), 42).

test(t_case_ret) :-
  fullsimple:type([], case(inl(true), {l, 1}, {r, 0}), int).

test(t_inr0) :- fullsimple:type([], inr(4), uni(_, int)).
test(t_inl0) :- fullsimple:type([], inl(true), uni(bool, _)).

test(t_case_let) :-
  Term = let({u, inr(4)}, case(u, {l, l}, {r, plus(r, r)})),
  fullsimple:type([], Term, int).

test(e_case_let0) :-
  Term = let({u, inr(4)}, case(u, {l, l}, {r, plus(r, r)})),
  fullsimple:eval([], Term, 8).

test(e_case_ite0) :-
  Term = case(inr(true), {l, 1}, {r, ite(r, 2, 3)}),
  fullsimple:eval([], Term, 2).

test(t_case_iszero) :-
  fullsimple:type([{x, nat}], case(x, {z, true}, {s(x1), false}), bool).
test(e_case_z) :-
  fullsimple:eval([{x, z}], case(x, {z, true}, {s(_), false}), true).
test(e_case_sz) :-
  fullsimple:eval([{x, s(z)}], case(x, {z, true}, {s(_), false}), false).

declare_iseven(
    lam(iseven,
        lam(n,
            case(n,
              {z, true},
              {s(n1),
                case(n1,
                  {z, false},
                  {s(n2), app(iseven, n2)})})))).

test(e_fix_iseven_z) :-
  declare_iseven(IsEvenF), IsEven = fix(IsEvenF),
  fullsimple:eval([], app(IsEven, z), true).
test(e_fix_iseven_sz) :-
  declare_iseven(IsEvenF), IsEven = fix(IsEvenF),
  fullsimple:eval([], app(IsEven, s(z)), false).
test(e_fix_iseven_ssssz) :-
  declare_iseven(IsEvenF), IsEven = fix(IsEvenF),
  fullsimple:eval([], app(IsEven, s(s(s(s(z))))), true).

%% Prelude
test(e_pre_iszero_z) :-
  fullsimple:evalp([{x, z}], app(iszero, x), true).
test(e_pre_iszero_sz) :-
  fullsimple:evalp([{x, s(z)}], app(iszero, x), false).
test(e_pre_iszero_lam, [fail]) :-
  fullsimple:evalp([{x, lam(y, y)}], app(iszero, x), false).

test(e_pre_pred_z) :-
  fullsimple:evalp([{n, z}], app(pred, n), z).
test(e_pre_pred_ssz) :-
  fullsimple:evalp([{n, s(s(z))}], app(pred, n), s(z)).
test(e_pre_int) :-
  Predi = lam(i, app(int_of_nat, app(pred, app(nat_of_int, i)))),
  fullsimple:evalp([{predi, Predi}], app(predi, 42), 41).

% test(e_fix_mult) :- Mult = lam(

:- end_tests(fullsimple).

% vim: syntax=prolog
