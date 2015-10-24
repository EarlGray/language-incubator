bnat(bo).
bnat(b0(X)) :- bnat(X).
bnat(b1(X)) :- bnat(X).

%% badd_unified(In1, In2, Out, Carry)
badd_unified(bo, bo, bo, b0(bo)).

badd_unified(b0(X), b0(Y), b0(Z), b0(bo)) :- badd_unified(X, Y, Z, b0(bo)).
badd_unified(b1(X), b0(Y), b1(Z), b0(bo)) :- badd_unified(X, Y, Z, b0(bo)).
badd_unified(b0(X), b1(Y), b1(Z), b0(bo)) :- badd_unified(X, Y, Z, b0(bo)).
badd_unified(b1(X), b1(Y), b0(Z), b1(bo)) :- badd_unified(X, Y, Z, b0(bo)).

badd_unified(b0(X), b0(Y), b1(Z), b0(bo)) :- badd_unified(X, Y, Z, b1(bo)).
badd_unified(b1(X), b0(Y), b0(Z), b1(bo)) :- badd_unified(X, Y, Z, b1(bo)).
badd_unified(b0(X), b1(Y), b0(Z), b1(bo)) :- badd_unified(X, Y, Z, b1(bo)).
badd_unified(b1(X), b1(Y), b1(Z), b1(bo)) :- badd_unified(X, Y, Z, b1(bo)).

int_bnat(0, 0, b0(bo)).
int_bnat(0, 1, b1(bo)).

int_bnat(N, I, b0) 
