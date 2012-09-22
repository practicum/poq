:- use_module(library(lists)).
:- use_module(library(assoc)).


:- write('        ~~~~ SCRATCH file opened').

a_descendant(X,Y) :- direct_child(X,Y).
a_descendant(X,Z) :- direct_child(X,Y),a_descendant(Y,Z).

direct_child(granddad,man1).
direct_child(granddad,man2).
direct_child(man2,grandkid).

% offzz(A) :- A.

say_hi :- write('saying hi').

uu(one) :- say_hi.
uu(two) :- say_hi.
uu(three) :- say_hi.

vv(_) :- demonat(_), uu(_).

% $something(X) :- uu(X).  % YAP prolog did not like the $ dollar sign in the predicate name


teacher_info( ebert,5627771225,[cecs1,cecs2,cecs3] ).

teacher_info( monge,5627771228,[cecs10,cecs12,cecs13] ).

teacher_info( gittleman,5627771227,[cecs21,cecs22,cecs23] ).

course_info( algorithms,cecs21,gittleman ).

course_info( systems_design,cecs22,gittleman ).

course_info( artificial_intelligence,cecs44,goldstein ).

taught_by( NUMBER, TEACHER ) :-
        course_info( _,NUMBER,TEACHER ).

taught_by( NUMBER, TEACHER ) :-
        teacher_info( TEACHER, _, LIST ),
        member( NUMBER, LIST ).

teaches( TEACHER, COURSE ) :-
        teacher_info( TEACHER, _, C_LIST ),
        member( COURSE, C_LIST ).

teaches( TEACHER, COURSE ) :-
        course_info( _,COURSE,TEACHER ).


/*
  example usage:

  ?- teaches( goldstein, X).
  X = cecs44.

  ?- teaches( gittleman, X).
  X = cecs21
  X = cecs22
  X = cecs23 .

  ?- taught_by( cecs44, X).
  X = goldstein .

  ?- taught_by( cecs12, X).
  X = monge .

  ?- taught_by( cecs3, X).
  X = ebert .

*/


% mmatch((A,B),B) :- write_canonical(B),nl.

siblings(a1,x1).
siblings(a2,x2).
siblings(a3,x3).

% DEMO of INFINITE tree
siblings(X,Y) :- siblings(Y,X).
% ?- siblings(X,Y) % produces a never-ending stream of assignments

