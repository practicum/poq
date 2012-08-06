:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(modules/small_lists).
:- use_module(modules/datatypes).

:- write('        ~~~~ test file opened').

mappingx(red,rojo).
mappingx(yellow,amarillo).
mappingx(blue,azul).

mapx([],[]).
mapx([X0|X1],[MX|R]) :-  mappingx(X0,MX),  mapx(X1,R).

redux(red,0).
redux(blue,2).
redux(yellow,4).

/*
  ... still not clear on 'is' versus '=:=' ...

?- 3 is 4 - 1.
true.

?- 3 = 4-1.
false.

?- 3 == 4-1.
false.

?- 3 =:= 4-1.
true.

?- X =:= 4-1.
ERROR: =:=/2: Arguments are not sufficiently instantiated
?- 4-1 =:= X.
ERROR: =:=/2: Arguments are not sufficiently instantiated

*/
reducex([],0). /* :- integer(0).*/
reducex([X0|X1],N) :- reducex(X1,M), redux(X0,P),  N is M + P.


