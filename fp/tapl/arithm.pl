%% typing rules
istype(nat).
istype(bool).

type(zero, nat).                        % [T-Zero]
type(succ(N), nat) :- type(N, nat).     % [T-Succ]
type(pred(N), nat) :- type(N, nat).     % [T-Pred]

type(true, bool).                       % [T-True]
type(false, bool).                      % [T-False]

type(iszero(T), bool) :- type(T, nat).  % [T-IsZero]
type(ite(T1, T2, T3), R) :- type(T1, bool), type(T2, R), type(T3, R).   % [T-If]

%% evaluation rules
eval1(zero, 0).                                 % [E-Zero]
eval1(succ(N), I) :- eval1(N, I1), I is I1 + 1. % [E-Succ]

eval1(true, true).                              % [E-True]
eval1(false, false).                            % [E-False]

eval1(ite(Cond, T, _), ET) :- eval1(Cond, true), eval1(T, ET).  % [E-IfTrue]
eval1(ite(Cond, _, T), ET) :- eval1(Cond, false), eval1(T, ET). % [E-IfFalse]

eval1(iszero(zero), true).                      % [E-IsZero]
eval1(iszero(succ(_)), false).                  % [E-IsNotZero]

