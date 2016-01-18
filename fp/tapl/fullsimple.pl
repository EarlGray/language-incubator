module(fullsimple, []).

istype(bool).
istype(nat).
istype(int).
istype(unit).
istype(arr(Ty1, Ty2)) :- istype(Ty1), istype(Ty2).
istype(pair(Ty1, Ty2)) :- istype(Ty1), istype(Ty2).

%% typing rules
% [T-Unit]
type(_, unit, unit).

% [T-True], [T-False]
type(_, true, bool).
type(_, false, bool).

% [T-If]
type(Ctx, ite(T1, T2, T3), Ty) :-
  type(Ctx, T1, bool),
  type(Ctx, T2, Ty),
  type(Ctx, T3, Ty).

% [T-Var]
type(Ctx, Var, Ty) :- atom(Var),
  memberchk({Var, Ty}, Ctx).

% [T-Zero], [T-Succ]
type(_, z, nat).
type(Ctx, s(N), nat) :- type(Ctx, N, nat).
type(Ctx, iszero(T), bool) :- type(Ctx, T, nat).

% [T-Abs]
type(Ctx, lam(X, T), arr(Ty1, Ty2)) :- atom(X),
  type([{X, Ty1} | Ctx], T, Ty2).

% [T-App]
type(Ctx, app(T1, T2), Ty) :-
  type(Ctx, T1, arr(Ty1, Ty)),
  type(Ctx, T2, Ty1).

% [T-Int]
type(_, Int, int) :- number(Int).

% [T-Seq]
type(Ctx, do([T]), Ty) :- type(Ctx, T, Ty).
type(Ctx, do([T | Ts]), Ty) :- type(Ctx, T, unit), type(Ctx, do(Ts), Ty).

% [T-As]
type(Ctx, as(T, Ty), Ty) :- type(Ctx, T, Ty).

% [T-Let]
type(Ctx, let({X, T1}, T2), Ty) :- atom(X),
  type(Ctx, T1, Ty1),
  type([{X, Ty1} | Ctx], T2, Ty).

% [T-Pair]
type(Ctx, pair(T1, T2), pair(Ty1, Ty2)) :-
  type(Ctx, T1, Ty1),
  type(Ctx, T2, Ty2).

% [T-Fst], [T-Snd]
type(Ctx, fst(T), Ty) :- type(Ctx, T, pair(Ty, _)).
type(Ctx, snd(T), Ty) :- type(Ctx, T, pair(_, Ty)).

% [T-Plus]
type(Ctx, plus(T1, T2), nat) :-
  type(Ctx, T1, nat), type(Ctx, T2, nat).
type(Ctx, plus(T1, T2), int) :-
  type(Ctx, T1, int), type(Ctx, T2, int).

%% evaluation rules
isnat(z).
isnat(s(_)).

eval(_, unit, unit).

eval(_, true, true).
eval(_, false, false).

eval(_, lam(X, T), lam(X, T)) :- atom(X).

eval(_, Num, Num) :- number(Num).

eval(_, z, z).
eval(_, s(N), s(N)).

eval(Vars, pair(T1, T2), pair(V1, V2)) :-
  eval(Vars, T1, V1),
  eval(Vars, T2, V2).

eval(Vars, fst(T), V) :- eval(Vars, T, pair(V, _)).
eval(Vars, snd(T), V) :- eval(Vars, T, pair(_, V)).

% [E-Var]
eval(Vars, Var, Val) :- atom(Var), !,
  memberchk({Var, Val}, Vars).

% [E-If]
eval(Vars, ite(Cond, T, _), V) :-
  eval(Vars, Cond, true), eval(Vars, T, V).
eval(Vars, ite(Cond, _, T), V) :-
  eval(Vars, Cond, false), eval(Vars, T, V).

% [E-App]
eval(Vars, app(T1, T2), V) :-
  eval(Vars, T1, lam(X, TLam)),
  eval(Vars, T2, V2),
  eval([{X, V2} | Vars], TLam, V).

% builtins:
eval(Vars, app(T1, T2), V) :-
  eval(Vars, T1, prolog(F)),
  eval(Vars, T2, V2),
  call(F, V2, V).

% [E-As]
eval(Vars, as(T, _), V) :- eval(Vars, T, V).

% [E-Let]
eval(Vars, let({X, T1}, T2), V) :-
  eval(Vars, T1, VX),
  eval([{X, VX} | Vars], T2, V).

eval(Vars, plus(T1, T2), V) :-
  eval(Vars, T1, X), number(X),
  eval(Vars, T2, Y), number(Y),
  V is X + Y.

eval(_Vars, plus(z, N), N).
eval(Vars, plus(s(M), N), s(N1)) :- eval(Vars, plus(M, N), N1).

eval(Vars, plus(T1, T2), V) :-
  eval(Vars, T1, X), isnat(X),
  eval(Vars, T2, Y), isnat(Y),
  eval(Vars, plus(X, Y), V).

eval(Vars, int_of_nat(T), I) :-
  eval(Vars, T, N),
  int_of_nat(N, I).

eval(Vars, nat_of_int(T), N) :-
  eval(Vars, T, I),
  nat_of_int(I, N).

eval(Vars, print(T), unit) :-
  eval(Vars, T, V),
  !, write(V).

%% builtins:
int_of_nat(z, 0).
int_of_nat(s(N1), I) :- int_of_nat(N1, I1), I is I1 + 1.

nat_of_int(I, _) :- I < 0, !, fail.
nat_of_int(0, z).
nat_of_int(I, s(N)) :- I1 is I - 1, nat_of_int(I1, N).

prelude_write(Term, unit) :- write(Term).

%% Prelude
prelude_defs([
  {id,          arr(X, X),      lam(x, x)},
  % {add,         arr(int, arr(int, int)),  lam(X, lam(y, plus(X, y))) },
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
