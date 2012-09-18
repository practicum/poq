:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(modules/small_lists).
:- use_module(modules/datatypes).

:- write('        ~~~~ SCRATCH file opened').

descendantxx(X,Y) :- offspringxx(X,Y).
descendantxx(X,Z) :- offspringxx(X,Y),descendantxx(Y,Z).

offspringxx(dadxx,ishxx).
offspringxx(dadxx,whoxx).
offspringxx(whoxx,grandkidxx). % if who is the parent, then 'solve' will not terminate. if 'ish' is used, then it succeeds. what??

% offzz(A) :- A.

say_hi :- write('saying hi').

uu(one) :- say_hi.
uu(two) :- say_hi.
uu(three) :- say_hi.

vv(_) :- demonat(_), uu(_).

% $something(X) :- uu(X).

solvexx(true).
solvexx((A,B)) :- solvexx(A), solvexx(B).
solvexx(A) :- clause(A,B), solvexx(B).

% mmatch((A,B),B) :- write_canonical(B),nl.