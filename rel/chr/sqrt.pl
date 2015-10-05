:- module(sqrt, [sqrt/2]).
:- use_module(library(chr)).

:- chr_constraint sqrt(?natural, ?natural).
sqrt(X,G) <=> D is G*G/X-1, abs(D)>0 | Y is (G+X/G) div 2, sqrt(X, Y).
