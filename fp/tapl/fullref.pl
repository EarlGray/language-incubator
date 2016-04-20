%% fullref
%
% <type> ::= arr(<type>, <type>) | unit | int | ref(<type>)
% <value> ::= lam(<var>, <term>) | unit | <int> | <loc>
%
% <term>
%   ::= <var>                       : type(ctx, store)
%     | <int>                       : int
%     | lam(<var:ty1>,<term:ty2>)   : arr(ty1, ty2)
%     | app(<term:arr(ty1,ty2)>, <term:ty1>)    : ty2
%     | unit                        : unit
%     | ref(<term:type>)            : ref(<type>)
%     | @<term:ref(<type>)>         : <type>
%     | set(<term>, <term>)         : unit
%     | do([<term:unit>, .., <term:ty>])       : ty
%
% Sugar:
%   let(<var>=<term1>, <term2:type>) => app(lam(<var>, <term2>), <term1>)

:- module(fullref, []).

vartype(Ctx, X, Ty) :- memberchk({X, Ty}, Ctx), !.
vartype(_, X, err({unknown_var, X})) :- !.

tcheck(err(Err), _, err(Err)) :- not(var(Err)), !.
tcheck(_, Val, Val) :- !.

textract1(err(Err), _, _, err(Err)) :- !.
textract1(T, Fun, _, Arg) :- functor(T, Fun, 1), arg(1, T, Arg), !.
textract1(_, _, Err, err(Err)) :- !.

%% Type relation: type(Store, Ctx, Term, Type).
% [T-Unit]
type(_, _, unit, unit) :- !.                
% [T-Int]
type(_, _, T, int) :- number(T), !.
% [T-Var]
type(_, Ctx, X, Ty) :- atom(X), vartype(Ctx, X, Ty), !.

type(Store, Ctx, ref(T), Ty) :- !,          % [T-Ref]
  type(Store, Ctx, T, TTy),
  tcheck(TTy, ref(TTy), Ty).
type(Store, Ctx, @T, Ty) :- !,              % [T-Deref]
  type(Store, Ctx, T, TTy),
  textract1(TTy, ref, {'T-Deref', TTy, T}, Ty).

type(Store, Ctx, set(T1, T2), Ty) :- !,   % [T-Assign]
  type(Store, Ctx, T1, RefTy),
  (RefTy = ref(Ty1) -> 
    type(Store, Ctx, T2, Ty2),
    (Ty2 = Ty1 -> Ty = unit ; Ty = err({'T-Assign, type mismatch', Ty1, Ty2}))
    ; Ty = err({'T-Assign, ref expected:', T1})).

type(Store, Ctx, do([T]), Ty) :- type(Store, Ctx, T, Ty), !.  % [T-Seq]
type(Store, Ctx, do([T | Ts]), Ty) :- !,
  type(Store, Ctx, T, unit),
  type(Store, Ctx, do(Ts), Ty).

type(Store, Ctx, lam(X, T), Ty) :- !,       % [T-Lam]
  Ctx1 = [{X, Ty1} | Ctx],
  type(Store, Ctx1, T, Ty2),
  tcheck(Ty2, arr(Ty1, Ty2), Ty).
type(Store, Ctx, app(T1, T2), Ty) :- !,     % [T-App]
  type(Store, Ctx, T1, arr(Ty1, Ty)),
  type(Store, Ctx, T2, Ty1).

% sugar:
type(Store, Ctx, let(X=T1, T2), Ty) :- !,
  type(Store, Ctx, app(lam(X, T2), T1), Ty).

%% Run-time store:
store(Heap) :- !, Heap = heap{newloc:0}.
store_get(Heap, loc(L), Val) :- Val = Heap.get(L).
store_set(Heap0, Heap1, loc(L), Val) :- Heap1 = Heap0.put(L, Val).
store_new(Heap0, Heap1, loc(Loc), Val) :-
  Loc = Heap0.get(newloc), NewLoc is Loc + 1,
  Heap01 = Heap0.put(newloc, NewLoc),
  Heap1 = Heap01.put(Loc, Val).

%% Eval relation: eval(Env, {Heap0, Term}, {Heap1, Value}).
eval(_, {Heap, unit}, {Heap, unit}) :- !.
eval(_, {Heap, Num}, {Heap, Num}) :- number(Num), !.
eval(Env, {Heap, Var}, {Heap, Val}) :- atom(Var), memberchk({Var, Val}, Env), !.
eval(_, {H, lam(X, T)}, {H, lam(X, T)}) :- !, atom(X).

% [E-App]
eval(Env, {Heap, app(T1, T2)}, {Heap1, Val}) :- !,
  eval(Env, {Heap, T2}, {Heap00, Varg}),
  eval(Env, {Heap00, T1}, {Heap01, lam(X, Tb)}),
  Env1 = [{X, Varg} | Env],
  eval(Env1, {Heap01, Tb}, {Heap1, Val}).
% [E-Deref]
eval(Env, {Heap0, @(T)}, {Heap1, Val}) :- !,
  eval(Env, {Heap0, T}, {Heap1, Loc}),
  store_get(Heap1, Loc, Val).
% [E-Assign]
eval(Env, {Heap0, set(TRef, TVal)}, {Heap1, unit}) :- !,
  eval(Env, {Heap0, TRef}, {Heap01, Loc}),
  eval(Env, {Heap01, TVal}, {Heap02, Val}),
  store_set(Heap02, Heap1, Loc, Val).
% [E-Ref]
eval(Env, {Heap0, ref(T)}, {Heap1, Loc}) :-
  eval(Env, {Heap0, T}, {Heap01, Val}),
  store_new(Heap01, Heap1, Loc, Val).

% desugar:
eval(Env, {H0, let(X=T1, T2)}, {H1, V}) :- !,
  eval(Env, {H0, app(lam(X, T2), T1)}, {H1, V}).

% vim: set syntax=prolog ts=2 sw=2
