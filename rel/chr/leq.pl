:- module(leq,[leq/2]).
:- use_module(library(chr)).

:- chr_constraint leq/2.
reflexivity  @ leq(X,X) <=> true.               %% may be removed
antisymmetry @ leq(X,Y), leq(Y,X) <=> X = Y.    %% simplify two rules into one
transitivity @ leq(X,Y), leq(Y,Z) ==> leq(X,Z). %% add the new rule , leq(X,Z)
idempotence  @ leq(X, Y) \ leq(X,Y) <=> true.   %% remove duplicate facts

%% :- chr_constraint mi
