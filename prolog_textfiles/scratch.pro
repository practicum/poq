:- use_module(library(lists)).
:- use_module(library(assoc)).


:- write('        ~~~~ SCRATCH file opened').

a_descendant(X,Y) :- direct_child(X,Y).
a_descendant(X,Z) :- direct_child(X,Y),a_descendant(Y,Z).

direct_child(granddad,man1).
direct_child(granddad,man2).
direct_child(man2,grandkid).

% offzz(A) :- A.

% $something(X) :- uu(X).  % YAP prolog did not like the $ dollar sign in the predicate name

% mmatch((A,B),B) :- write_canonical(B),nl.

siblings(a1,x1).
siblings(a2,x2).
siblings(a3,x3).

% DEMO of INFINITE tree
siblings(X,Y) :- siblings(Y,X).
% ?- siblings(X,Y) % produces a never-ending stream of assignments

demo_incomplete_struct(OUT) :-
        OUT = [a,b,c|X],
        patch_with_THING(X,THING), % X will 'share' with prior line.
        THING = [z,y,x].               % THING will 'share' with prior line.

patch_with_THING(T,T).


demo_incomplete_struct2(OUT) :-
        OUT = [a,b,c|X],
        patch_with_THING2(X,THING), % X will 'share' with prior line.
        THING = [].               % THING will 'share' with prior line.

patch_with_THING2([huh|T],T).


demo_incomplete_struct3(OUT) :-
        OUT = [a,b,c|Hole1],
        patch_with_THING3(Hole1,_Hole2). % X will 'share' with prior line.


patch_with_THING3([huh,wow,wut|Hole],Hole).


demo_incomplete_struct4(OUT) :-
        OUT = [a,b,c|Hole1],          % as long as we LEAVE A PLACEHOLDER
        Hole1 = [huh,wow,wut|_Hole2]. % then we can instantiate the placeholder to whatever, and 'Hole1' is *sharing* with prior line.


/*
now we make a difference list that looks like:
  DATA_LIST-ENDNAME

if both are the same, then essentially we assume we have:
  ENDNAME-ENDNAME
    ... which of course would be an empty list, because all it contains is its own end
*/

sample_concat( A1-Z1, Z1-Z2, A1-Z2 ).

/*
?- sample_concat( [a,b,c|T1]-T1,[d,e|T2]-T2,L).
T1 = [d, e|T2],
L = [a, b, c, d, e|T2]-T2.
*/
