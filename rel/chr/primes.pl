:- module(primes, [prime/1, upto/1]).
:- use_module(library(chr)).

:- chr_constraint prime(?natural).
sift @ prime(I) \ prime(J) <=> J mod I =:= 0 | true.

:- chr_constraint upto(?natural).
upto(1) <=> true.
upto(N) <=> N > 1 | N1 is N - 1, prime(N), upto(N1).

