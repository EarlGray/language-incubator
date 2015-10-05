:- module(gcd1,[gcd1/1]).
:- use_module(library(chr)).

:- chr_constraint gcd1(?natural).
%% gcd1(M) \ gcd1(N) <=> odd(M), even(N) | gcd1(N div 2).
gcd1(N) \ gcd1(M) <=> 0<N,N=<M | Z is M mod N, gcd1(Z).
gcd1(0) <=> true.
