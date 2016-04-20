% fullsimple:
%
% <type> ::= bool
%          | nat | int | unit
%          | <type> -> <type> | <type> * <type> | <type>+<type>
%          | {<var>:<type>, ... }
% <term> ::= true | false                                     : bool
%         | <nat>                                             : nat
%         | unit                                              : unit
%         | ite(<term1:bool>, <term2:type>, <term3:type>)     : type
%         | <var>                                             : type(ctx)
%         | lam(<var:t1>, <term:t2>)                          : t1 -> t2
%         | app(<term1:t1->t2>, <term2:t1>)                   : t2
%         | do([<term:unit>, <term:unit>, ..., <term:type>])  : type
%         | as(<term>, <type>)                                : type
%         | let({<var>, <term>}, <term:type>)                 : type
%         | plus(<term:nat>, <term:nat>)                      : nat
%         | plus(<term:int>, <term:int>)                      : int
%         | pair(<term:t1>, <term:t2>)                        : t1 * t2
%         | fst(<term:pair>) | snd(<term:pair>)
%         | inl(<term:t1>) | inr(<term:t2>)                   : t1 + t2
%         | case(<term:t1+t2>,
%             {inl(<var:t1>), <term:type>},
%             {inr(<var:t2>), <term:type>})                   : type
%         | case(<term:nat>,
%             {z, <term:rty>},
%             {s(<var:vty>), <term:rty>})                     : rty
%         | fix(<term:type->type>)                            : type
%         | letrec({<var>, <term>}, <term:type>)              : type
%         | { <var1>=<term:ty1>, ...}                         : { <var1>:<ty1>, ...}
%         | <term:{.., <var>:<ty>, ..}>/<var>                 : ty
%
% <nat> ::= z | s(<term>)
%
% + a small prelude
%
:- module(fullsimple, []).

istype(bool).
istype(nat).
istype(int).
istype(unit).
istype(arr(Ty1, Ty2)) :- istype(Ty1), istype(Ty2).
istype(pair(Ty1, Ty2)) :- istype(Ty1), istype(Ty2).
istype(uni(Ty1, Ty2)) :- istype(Ty1), istype(Ty2).

%% typing rules
% [T-Unit]
type(_, unit, unit) :- !.

% [T-True], [T-False]
type(_, true, bool) :- !.
type(_, false, bool) :- !.

% [T-If]
type(Ctx, ite(T1, T2, T3), Ty) :- !,
  type(Ctx, T1, bool),
  type(Ctx, T2, Ty),
  type(Ctx, T3, Ty).

% [T-Var]
type(Ctx, Var, Ty) :- isvar(Var), !,
  memberchk({Var, Ty}, Ctx).

% [T-Zero], [T-Succ]
type(_, z, nat) :- !.
type(Ctx, s(N), nat) :- type(Ctx, N, nat), !.

% [T-Lam]
type(Ctx, lam(X, T), arr(Ty1, Ty2)) :- isvar(X), !,
  type([{X, Ty1} | Ctx], T, Ty2).

% [T-App]
type(Ctx, app(T1, T2), Ty) :- !,
  type(Ctx, T1, arr(Ty1, Ty)),
  type(Ctx, T2, Ty1).

% [T-Int]
type(_, Int, int) :- number(Int), !.

% [T-Seq]
type(Ctx, do([T]), Ty) :- type(Ctx, T, Ty).
type(Ctx, do([T | Ts]), Ty) :- type(Ctx, T, unit), type(Ctx, do(Ts), Ty).

% [T-As]
type(Ctx, as(T, Ty), Ty) :- !, type(Ctx, T, Ty).

% [T-Let]
type(Ctx, let({X, T1}, T2), Ty) :- isvar(X), !,
  type(Ctx, T1, Ty1),
  CtxL = [{X, Ty1} | Ctx], type(CtxL, T2, Ty).

% [T-Pair]
type(Ctx, pair(T1, T2), pair(Ty1, Ty2)) :- !,
  type(Ctx, T1, Ty1), type(Ctx, T2, Ty2).

% [T-Fst], [T-Snd]
type(Ctx, fst(T), Ty) :- !, type(Ctx, T, pair(Ty, _)).
type(Ctx, snd(T), Ty) :- !, type(Ctx, T, pair(_, Ty)).

% [T-Plus]
type(Ctx, plus(T1, T2), nat) :- type(Ctx, T1, nat), !,
  type(Ctx, T2, nat).
type(Ctx, plus(T1, T2), int) :- !,
  type(Ctx, T1, int), type(Ctx, T2, int).

% [T-Inl], [T-Inr], [T-Case]
type(Ctx, inl(T), uni(Ty, _)) :- !, type(Ctx, T, Ty).
type(Ctx, inr(T), uni(_, Ty)) :- !, type(Ctx, T, Ty).
type(Ctx, case(T0, {z, Tz}, {s(V), Ts}), Ty) :- !,
  isvar(V),
  type(Ctx, T0, nat),
  type(Ctx, Tz, Ty),
  CtxS = [{V, nat} | Ctx], type(CtxS, Ts, Ty).
type(Ctx, case(T0, {inl(Vl), Tl}, {inr(Vr), Tr}), Ty) :- !,
  isvar(Vl), isvar(Vr),
  type(Ctx, T0, uni(Ty1, Ty2)),
  Ctx1 = [{Vl, Ty1} | Ctx], type(Ctx1, Tl, Ty),
  Ctx2 = [{Vr, Ty2} | Ctx], type(Ctx2, Tr, Ty).

% [T-Fix], [T-Letrec]
type(Ctx, fix(T), Ty) :- !,
  type(Ctx, T, arr(Ty, Ty)).
type(Ctx, letrec({X, T1}, T2), Ty) :- !,
  type(Ctx, let({X, fix(lam(X, T1))}, T2), Ty). % just syntactic sugar for fix

% [T-Rec], [T-RecProj]
type(Ctx, {Ts}, {Tys}) :- !, rectype(Ctx, Ts, Tys).
% a shortcut for field(T, F), '/' used because it is left-accociative
type(Ctx, T/F, Ty) :- !, type(Ctx, field(T, F), Ty).
type(Ctx, field(T, F), Ty) :- !, isvar(F),
  type(Ctx, T, {RecTy}), fieldtype(RecTy, F, Ty).

% Typing Error
type(Ctx, T, Ty) :-
  format(user_error,
    'Error:\n  can\'t unify:\t~p\n  with type:\t~p\n  in context:\t~p\n',
    [T, Ty, Ctx]),
  fail.

tmember(V:_Ty, V) :- !.
tmember((V:_Ty, _), V) :- !.
tmember((_:_Ty, Vs), V) :- tmember(Vs, V).

fieldtype(V:Ty, V, Ty) :- !.
fieldtype((V:Ty, _), V, Ty) :- !.
fieldtype((_:_, Vs), V, Ty) :- fieldtype(Vs, V, Ty).

rectype(Ctx, V=T, V:Ty) :- !, type(Ctx, T, Ty).
rectype(Ctx, (V=T, Ts), (V:Ty, Tys)) :- !,
  type(Ctx, T, Ty),
  rectype(Ctx, Ts, Tys).

%% evaluation rules
isnat(z).
isnat(s(_)).

isvar(z) :- !, fail.
isvar(true) :- !, fail.
isvar(false) :- !, fail.
isvar(V) :- atom(V).

eval(_, unit, unit) :- !.

eval(_, true, true) :- !.
eval(_, false, false) :- !.

eval(_, lam(X, T), lam(X, T)) :- !, isvar(X).

eval(_, Num, Num) :- number(Num), !.

eval(_, z, z) :- !.
eval(Vars, s(T), s(N)) :- !, eval(Vars, T, N).

eval(Vars, pair(T1, T2), pair(V1, V2)) :- !,
  eval(Vars, T1, V1),
  eval(Vars, T2, V2).

eval(Vars, fst(T), V) :- !, eval(Vars, T, pair(V, _)).
eval(Vars, snd(T), V) :- !, eval(Vars, T, pair(_, V)).

% [E-Var]
eval(Vars, Var, Val) :- isvar(Var), !,
  memberchk({Var, Val}, Vars).

% [E-If]
eval(Vars, ite(Cond, T, _), V) :- eval(Vars, Cond, true), !,
  eval(Vars, T, V).
eval(Vars, ite(Cond, _, T), V) :- !,
  eval(Vars, Cond, false), eval(Vars, T, V).

% [E-App]
eval(Vars, app(T1, T2), V) :- eval(Vars, T1, lam(X, TLam)), !,
  eval(Vars, T2, V2),
  eval([{X, V2} | Vars], TLam, V).

% [E-AppBuiltin]
eval(Vars, app(T1, T2), V) :- !,
  eval(Vars, T1, prolog(F)),
  eval(Vars, T2, V2),
  % a builtin must take one argument and unify the result:
  call(F, V2, V).

% [E-As]
eval(Vars, as(T, _), V) :- !, eval(Vars, T, V).

% [E-Let]
eval(Vars, let({X, T1}, T2), V) :- !,
  eval(Vars, T1, VX),
  eval([{X, VX} | Vars], T2, V).

% [E-Plus]
eval(_Vars, plus(z, N), N).
eval(Vars, plus(s(M), N), s(N1)) :- !, eval(Vars, plus(M, N), N1).

eval(Vars, plus(T1, T2), V) :-
  eval(Vars, T1, X), number(X), !,
  eval(Vars, T2, Y), number(Y),
  V is X + Y.

eval(Vars, plus(T1, T2), V) :-
  eval(Vars, T1, X), isnat(X), !,
  eval(Vars, T2, Y), isnat(Y),
  eval(Vars, plus(X, Y), V).

% [E-Inl], [E-Inr]
eval(Vars, inl(T), inl(V)) :- !, eval(Vars, T, V).
eval(Vars, inr(T), inr(V)) :- !, eval(Vars, T, V).
% [E-CaseInl], [E-CaseInr], [E-Case]
eval(Vars, case(T0, {z, Tz}, {s(_), _}), Val) :-
  eval(Vars, T0, z), !, eval(Vars, Tz, Val).
eval(Vars, case(T0, {z, _}, {s(V), Ts}), Val) :-
  !, eval(Vars, T0, s(N)), Vars1 = [{V, N} | Vars],
  eval(Vars1, Ts, Val).
eval(Vars, case(T0, {inl(Var), TL}, {inr(_), _}), Val) :-
  eval(Vars, T0, inl(V0)),
  !, Vars1 = [{Var, V0} | Vars],
  eval(Vars1, TL, Val).
eval(Vars, case(T0, {inl(_), _}, {inr(Var), TR}), Val) :-
  eval(Vars, T0, inr(V0)),
  !, Vars1 = [{Var, V0} | Vars],
  eval(Vars1, TR, Val).

% [E-Fix], [E-Letrec]
eval(Vars, fix(Tf), V) :- !,
  eval(Vars, Tf, Tl), Tl = lam(X, Tb),
  subst(X, fix(Tl), Tb, TbS),
  eval(Vars, TbS, V).
eval(Vars, letrec({X, T1}, T2), V) :- !,
  eval(Vars, let({X, fix(lam(X, T1))}, T2), V).

% [E-Rec], [E-Recfld]
eval(Vars, {Ts}, {Vs}) :- !, receval(Vars, Ts, Vs).
eval(Vars, T/F, V) :- !, eval(Vars, field(T, F), V). % a shortcut
eval(Vars, field(T, F), V) :- !, isvar(F),
  eval(Vars, T, {VT}),
  recget(VT, F, V).

eval(Vars, T, _) :-
  format(user_error, 'Evaluation stuck\n  on term:\t~p\n  with vars:\t~p', [T, Vars]), fail.

receval(Vars, X=T, X=V) :- !, eval(Vars, T, V).
receval(Vars, (X=T, Ts), (X=V, Vs)) :- !,
  eval(Vars, T, V), receval(Vars, Ts, Vs).

recget(F=V, F, V) :- !.
recget((F=V, _), F, V) :- !.
recget((_=_, Vs), F, V) :- !, recget(Vs, F, V).

%% naive subst(What, With, Where, Result)
subst(What, _, _, _) :- not(atom(What)), !, fail.
subst(What, With, What, With) :- !.
subst(V, With, lam(V, Tb), lam(With, TbS)) :- !,
  subst(V, With, Tb, TbS). % here be dragonbugs
subst(_, _, [], []) :- !.
subst(V, With, [T | Ts], [TS | TSs]) :- !,
  subst(V, With, T, TS),
  subst(V, With, Ts, TSs).
subst(V, With, as(T, Ty), as(Ts, Ty)) :- !,
  subst(V, With, T, Ts).
subst(V, With, let({X, Tx}, Tl), let({X, TxS}, TlS)) :- X \= V, !,
  subst(V, With, Tx, TxS), subst(V, With, Tl, TlS).
subst(V, W, case(V, {z, Tz}, {s(Y), Ts}), case(W, {z, TzS}, {s(Y), TsS})) :- !,
  subst(V, W, Tz, TzS), subst(V, W, Ts, TsS).
subst(V, W, case(X, {z, Tz}, {s(V), Ts}), case(X, {z, TzS}, {s(W), TsS})) :- !,
  subst(V, W, Tz, TzS), subst(V, W, Ts, TsS).
subst(V, W, case(X, {z, Tz}, {s(Y), Ts}), case(X, {z, TzS}, {s(Y), TsS})) :- !,
  subst(V, W, Tz, TzS), subst(V, W, Ts, TsS).
subst(V, W, Fun, FunS) :- functor(Fun, Name, 1), !,
  arg(1, Fun, Arg), subst(V, W, Arg, ArgS),
  functor(FunS, Name, 1), arg(1, FunS, ArgS).
subst(V, W, Fun, FunS) :- functor(Fun, Name, 2), !,
  arg(1, Fun, Arg1), subst(V, W, Arg1, Arg1S),
  arg(2, Fun, Arg2), subst(V, W, Arg2, Arg2S),
  functor(FunS, Name, 2),
  arg(1, FunS, Arg1S), arg(2, FunS, Arg2S).
subst(V, W, Fun, FunS) :- functor(Fun, Name, 3), !,
  arg(1, Fun, Arg1), subst(V, W, Arg1, Arg1S),
  arg(2, Fun, Arg2), subst(V, W, Arg2, Arg2S),
  arg(3, Fun, Arg3), subst(V, W, Arg3, Arg3S),
  functor(FunS, Name, 3),
  arg(1, FunS, Arg1S), arg(2, FunS, Arg2S), arg(3, FunS, Arg3S).
subst(_, _, What, What).

%
int_of_nat(z, 0) :- !.
int_of_nat(s(N1), I) :- !, int_of_nat(N1, I1), I is I1 + 1.

nat_of_int(I, _) :- I < 0, !, fail.
nat_of_int(0, z) :- !.
nat_of_int(I, s(N)) :- !, I1 is I - 1, nat_of_int(I1, N).

prelude_write(Term, unit) :- write(Term).

%% Prelude
prelude_defs([
  {id,          arr(X, X),      lam(x, x)},
  {iszero,      arr(nat, bool), lam(x, case(x, {z, true}, {s(any), false}))},
  {pred,        arr(nat, nat),  lam(n, case(n, {z, z}, {s(n1), n1}))},
  {nat_of_int,  arr(int, nat),  prolog(nat_of_int)},
  {int_of_nat,  arr(nat, int),  prolog(int_of_nat)},
  {print,       arr(_, unit),   prolog(prelude_write)}
]).

% eval with Prelude
evalp(Vars, Term, Val) :-
  prelude_vars(PreVars),
  append(Vars, PreVars, Vars1),
  eval(Vars1, Term, Val).

% type with Prelude
typep(Ctx, Term, Ty) :-
  prelude_types(PreCtx),
  append(Ctx, PreCtx, Ctx1),
  type(Ctx1, Term, Ty).

prelude_vars(Pre) :-
  prelude_defs(P), prelude_vars(Pre, P).

prelude_vars([], []).
prelude_vars([{Name, Val} | Pre], [{Name, _Ty, Val} | Defs]) :-
  prelude_vars(Pre, Defs).

prelude_types(PreTys) :-
  prelude_defs(P), prelude_types(PreTys, P).

prelude_types([], []).
prelude_types([{Name, Type} | Pre], [{Name, Type, _Val} | Defs]) :-
  prelude_types(Pre, Defs).

% vim: syntax=prolog
