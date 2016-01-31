%% fullref
% <type>
%   ::= arr(<type>, <type>)
%     | unit
%     | ref(<type>)
%
% <term>
%   ::= <var>                       : type(ctx, store)
%     | lam(<var:ty1>,<term:ty2>)   : arr(ty1, ty2)
%     | app(<term:arr(ty1,ty2)>, <term:ty1>)    : ty2
%     | unit                        : unit
%     | ref(<term:type>)            : ref(<type>)
%     | @<term:ref(<type>)>         : <type>
%     | set(<term>, <term>)         : unit

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

type(Store, Ctx, lam(X, T), Ty) :- !,
  Ctx1 = [{X, Ty1} | Ctx],
  type(Store, Ctx1, T, Ty2),
  tcheck(Ty2, arr(Ty1, Ty2), Ty).

%% Eval relation: 
eval(_, _, unit, unit) :- !.
eval(_, Ctx, Var, Val) :- atom(Var), memberchk({Var, Val}, Ctx), !.

% vim: set syntax=prolog ts=2 sw=2
