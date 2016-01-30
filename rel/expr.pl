% DCG
expr(plus(A, B)) --> [+], expr(A), expr(B).
expr(mul(A,B)) --> [*], expr(A), expr(B).
expr(Num) --> num(Num).
expr(Xer) --> xer(Xer).

xer(var(x)) --> [x].
xer(pow(var(x), N)) --> [^], [x], num(N).

num(num(2)) --> [2].
num(num(3)) --> [3].
num(num(4)) --> [4].
num(num(5)) --> [5].

% parsing with append 
not_space(C) :- not(char_type(C, space)).

between(Before, Inside, After, In) :- 
  append(Before, Tail, In), append(Inside, After, Tail).

break(Cond, Head, Tail, In) :-
  append(Head, [C | Rest], In),
  forall(member(S, Head), apply(Cond, S)),
  not(call(Cond, C)), Tail = [C | Rest].

%many(Cond, Items, In) :-

parse_form(Lst, Form) :-
  between(['('], Inside, [')'], Lst),
  while(not_space, Form, Inside).

% vim: set syntax=prolog ts=2 sw=2
