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
  fullsimple:type([], case(inl(true), {inl(l), 1}, {inr(r), 0}), int).

test(t_uni_inr0) :- fullsimple:type([], inr(4), uni(_, int)).
test(t_uni_inl0) :- fullsimple:type([], inl(true), uni(bool, _)).
test(t_uni_infer) :-
  fullsimple:type([{x, Ty1}], case(x, {inl(n), n}, {inr(b), ite(b, 0, 1)}), Ty2),
  Ty1 = uni(int, bool), Ty2 = int.
test(t_case_let) :-
  Term = let({u, inr(4)}, case(u, {inl(l), l}, {inr(r), plus(r, r)})),
  fullsimple:type([], Term, int).

test(e_case_let0) :-
  Term = let({u, inr(4)}, case(u, {inl(l), l}, {inr(r), plus(r, r)})),
  fullsimple:eval([], Term, 8).
test(e_case_ite0) :-
  Term = case(inr(true), {inl(l), 1}, {inr(r), ite(r, 2, 3)}),
  fullsimple:eval([], Term, 2).

test(t_case_iszero) :-
  fullsimple:type([{x, nat}], case(x, {z, true}, {s(x1), false}), bool).
test(e_case_z) :-
  fullsimple:eval([{x, z}], case(x, {z, true}, {s(_), false}), true).
test(e_case_sz) :-
  fullsimple:eval([{x, s(z)}], case(x, {z, true}, {s(_), false}), false).

declare_iseven(
    lam(n,
        case(n,
          {z, true},
          {s(n1),
            case(n1,
              {z, false},
              {s(n2), app(iseven, n2)})}))).

test(e_fix_iseven_z) :-
  declare_iseven(IsEvenF), IsEven = fix(lam(iseven, IsEvenF)),
  fullsimple:eval([], app(IsEven, z), true).
test(e_fix_iseven_sz) :-
  declare_iseven(IsEvenF), IsEven = fix(lam(iseven, IsEvenF)),
  fullsimple:eval([], app(IsEven, s(z)), false).
test(e_fix_iseven_ssssz) :-
  declare_iseven(IsEvenF), IsEven = fix(lam(iseven, IsEvenF)),
  fullsimple:eval([], app(IsEven, s(s(s(s(z))))), true).

test(t_letrec_iseven) :- declare_iseven(IsEven),
  fullsimple:type([], letrec({iseven, IsEven}, iseven), arr(nat, bool)).
test(e_letrec_iseven_4) :- declare_iseven(IsEven),
  fullsimple:evalp([], letrec({iseven, IsEven}, app(iseven, app(nat_of_int, 4))), true).
test(e_letrec_iseven_sz) :- declare_iseven(IsEven),
  fullsimple:eval([], letrec({iseven, IsEven}, app(iseven, s(z))), false).

test(t_rec_intint) :-
  fullsimple:type([], {x=2, y=3}, {x:int, y:int}).
test(t_rec_1) :-
  fullsimple:type([], {x=3}, {x:int}).
test(t_rec_1eq, [fail]) :-
  fullsimple:type([], x=3, _).
test(t_recfld_point_x) :-
  fullsimple:type([{point, {x:int, y:int}}], point/x, int).
test(t_rec_nested) :-
  Term = let({p1, {x=2, y=3}},
           let({p2, {x=5, y=6}},
             {start=p1, end=p2})),
  fullsimple:type([], Term, {start:{x:int, y:int}, end:{x:int, y:int}} ).
test(t_recfld_nested) :-
  Term = let({p1, {x=2, y=3}},
           let({p2, {x=5, y=6}},
             {start=p1, end=p2})),
  fullsimple:type([], let({line, Term}, line/start/x), int).

test(e_recfld) :-
  fullsimple:eval([{point, {x=2, y=3}}], point/y, 3).
test(e_recfld_nested) :-
  Line = {start={x=0, y=0}, end={x=5, y=6}},
  fullsimple:eval([{line, Line}], line/end/y, 6).
test(e_rec_recfld) :-
  fullsimple:eval([], {x=8, y=6}/x, 8).

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

test(subst_nonatom, [fail]) :- fullsimple:subst(12, 13, 12, 13).
test(subst_atom_id) :- fullsimple:subst(x, y, x, y).
test(subst_atom_no) :- fullsimple:subst(x, y, z, z).
test(subst_num_id)  :- fullsimple:subst(x, 12, x, 12).
test(subst_num_no)  :- fullsimple:subst(x, 12, y, y).

:- end_tests(fullsimple).

% vim: syntax=prolog
