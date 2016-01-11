%% arr + bool
istype(bool).
istype(arr(Ty1, Ty2)) :- istype(Ty1), istype(Ty2).

isvar(X) :- atom(X).

%% typing contexts
ctx_intro(Ctx1, Ctx, {X, Ty}) :- Ctx1 = [{X, Ty} | Ctx].
ctx_elem(Ctx, {X, Ty}) :- memberchk({X, Ty}, Ctx).

%% typing rules
type(_, true, bool).
type(_, false, bool).

type(Ctx, X, Ty1) :- isvar(X),
  ctx_elem(Ctx, {X, Ty1}).

type(Ctx, lam(X, T), arr(Ty1, Ty2)) :-
  isvar(X),
  ctx_intro(Ctx1, Ctx, {X, Ty1}),
  type(Ctx1, T, Ty2).

type(Ctx, ite(T1, T2, T3), Ty) :-
  type(Ctx, T1, bool),
  type(Ctx, T2, Ty),
  type(Ctx, T3, Ty).

type(Ctx, app(T1, T2), Ty) :-
  type(Ctx, T1, arr(Ty1, Ty)),
  type(Ctx, T2, Ty1).

%% evaluation rules
eval(_, true, true).
eval(_, false, false).
eval(_, lam(X, T), lam(X, T)).

eval(Vars, ite(TCond, Then, _), V) :-
  eval(Vars, TCond, true), eval(Vars, Then, V).
eval(Vars, ite(TCond, _, Else), V) :-
  eval(Vars, TCond, false), eval(Vars, Else, V).
  
eval(Vars, X, V) :- isvar(X), memberchk({X, V}, Vars).
eval(Vars, app(Abs, T2), V) :-
  eval(Vars, Abs, lam(X, T1)),
  eval(Vars, T2, V2),
  eval([{X, V2} | Vars], T1, V).

%% some Church values
ch_id(lam(X, X)).

ch_pair(lam(X, lam(Y, lam(Z, app(app(Z, X), Y))))).
ch_fst(lam(X, lam(_Y, X))).
ch_snd(lam(_X, lam(Y, Y))).

% vim: syntax=prolog
