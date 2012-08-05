/*
A module file is a file holding a module/2 directive as its first
term. The module/2 directive declares the name and the public (i.e.,
externally visible) predicates of the module.

Below is an example of a module file, defining reverse/2 and hiding
the helper predicate rev/3.

A module can use all built-in predicates and, by default, cannot
redefine system predicates.

:- module(reverse, [reverse/2]).

reverse(List1, List2) :-
        rev(List1, [], List2).

rev([], List, List).
rev([Head|List1], List2, List3) :-
        rev(List1, [Head|List2], List3).
*/

:- module(smallnum,
	  [forcenat/1,
	   size_0_to_5/1,
	   size_1_to_5/1,
	   size_1_to_3/1]).


forcenat(0).
forcenat(1).
forcenat(2).
forcenat(3).
forcenat(4).
forcenat(5).
forcenat(6).
forcenat(7).
forcenat(8).
forcenat(9).
forcenat(10).
%forcenat(N) :- forcenat(M), N is M + 1. % we need to avoid this for now. too much infinite looping!


size_0_to_5(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5).

size_1_to_5(LIST) :- length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5).


size_1_to_3(LIST) :- length(LIST,1);length(LIST,2);length(LIST,3).