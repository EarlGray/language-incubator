%% typing rules
type(zero, nat).
type(succ(N), nat) :- type(N, nat).
type(pred(N), nat) :- type(N, nat).

type(true, bool).
type(false, bool).

type(iszero(T), bool) :- type(T, nat).
type(ite(T1, T2, T3), R) :- type(T1, bool), type(T2, R), type(T3, R).

%% evaluation rules
eval1(zero, 0).
eval1(succ(N), I) :- eval1(N, I1), I is I1 + 1.

eval1(true, true).
eval1(false, false).

eval1(ite(Cond, T, _), ET) :- eval1(Cond, true), eval1(T, ET).
eval1(ite(Cond, _, T), ET) :- eval1(Cond, false), eval1(T, ET).

eval1(iszero(zero), true).
eval1(iszero(succ(_)), false).

