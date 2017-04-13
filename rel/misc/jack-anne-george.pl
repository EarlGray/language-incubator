% vim: set syntax=prolog

married(X) :- married_or_unmarried(X).
unmarried(X) :- married_or_unmarried(X).

married_or_unmarried(anne).
married(jack).
unmarried(george).

looks(jack, anne).
looks(anne, george).

married_looks_to_unmarried(X, Y) :-
    married(X), unmarried(Y),
    looks(X, Y).
