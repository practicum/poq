:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(modules/small_lists).
:- use_module(modules/datatypes).

:- write('        ~~~~ SCRATCH file opened').

descendant(X,Y) :- offspring(X,Y).
descendant(X,Z) :- offspring(X,Y),descendant(Y,Z).

offspring(dad,ish).
offspring(dad,who).
offspring(who,grandkid). % if who is the parent, then 'solve' will not terminate. if 'ish' is used, then it succeeds. what??

offzz(A) :- A.

say_hi :- write('saying hi').

uu(one) :- say_hi.
uu(two) :- say_hi.
uu(three) :- say_hi.

vv(_) :- demonat(_), uu(_).

$something(X) :- uu(X).

solve(true).
solve((A,B)) :- solve(A), solve(B).
solve(A) :- clause(A,B), solve(B).

mmatch((A,B),B) :- write_canonical(B),nl.