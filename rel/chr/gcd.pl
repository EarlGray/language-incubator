:- module(gcd,[gcd/1]).
:- use_module(library(chr)).

:- chr_constraint gcd(?natural).
gcd(X), gcd(Y) <=> X > Y | Z is X - Y, gcd(Z), gcd(Y).
gcd(X), gcd(Y) <=> X < Y | Z is Y - X, gcd(X), gcd(Z).
gcd(X), gcd(X) <=> gcd(X).
