:- module(fib,[naive_fib/2,mem_fib/2,upto_fib/1,upto_fib1/1,tlrec_fib/2]).
:- use_module(library(chr)).

%% plain exponential recursive check
:- chr_constraint naive_fib(?natural, ?natural).
f0  @ naive_fib(0, M) ==> M = 1.
f1  @ naive_fib(1, M) ==> M = 1.
fn  @ naive_fib(N, M) ==> N>=2 |
      N1 is N-1, N2 is N-2, naive_fib(N1, M1), naive_fib(N2, M2), M is M1+M2.

%% recursive with memoization
:- chr_constraint mem_fib(?natural, ?natural).
mem @ mem_fib(N, M1) \ mem_fib(N, M2) <=> M1=M2.
f0  @ mem_fib(0, M) ==> M = 1.
f1  @ mem_fib(1, M) ==> M = 1.
fn  @ mem_fib(N, M) ==> N>=2 |
        N1 is N-1, N2 is N-2, mem_fib(N1, M1), mem_fib(N2, M2), M is M1+M2.

%% grow constraint store with all fibs from 0 up to N
:- chr_constraint fib(?natural, ?natural).
:- chr_constraint upto_fib(?natural).
f01 @ upto_fib(_Max) ==> fib(0,1), fib(1,1).
fn  @ upto_fib(Max), fib(N1,M1), fib(N2,M2) ==>
        Max>N2, N11 is N1+1, N2=:=N11 | N21 is N2 + 1, M3 is M1+M2, fib(N21, M3).

%% keep only 2 last numbers
:- chr_constraint upto_fib1(?natural).
f01 @ upto_fib1(_Max) ==> fib(0,1), fib(1,1).
fn  @ upto_fib1(Max), fib(N2,M2) \ fib(N1,M1) <=>
        Max>N2, N11 is N1+1, N2=:=N11
        | N21 is N2+1, M3 is M1+M2, fib(N21, M3).

%% "tail-recursive" version
:- chr_constraint tlrec_fib(?natural, ?natural).
:- chr_constraint tlrec_fib(?natural, ?natural, ?natural, ?natural, ?natural).
f01 @ tlrec_fib(Max,X) <=> tlrec_fib(Max,1,1,1,X).
f_i @ tlrec_fib(Max,N,M1,M2,X) <=> Max>N |
        N1 is N+1, M3 is M1+M2, tlrec_fib(Max, N1, M2, M3, X).
f_n @ tlrec_fib(_,_,_,Val,X) <=> X = Val.

