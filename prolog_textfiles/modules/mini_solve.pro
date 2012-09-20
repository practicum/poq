
:- module(mini_solve,
          [mini_solve/1]).

           /*
           size_0_to_1/1,
           size_1_to_6/1]).
             */

/*
a_descendant(X,Y) :- direct_child(X,Y).
a_descendant(X,Z) :- direct_child(X,Y),a_descendant(Y,Z).

direct_child(granddad,man1).
direct_child(granddad,man2).
direct_child(man2,grandkid).
*/

/*
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
*/

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



/*
  Exit: (10) clause
  (  (direct_child(man1, _G443), a_descendant(_G443, grandkid)),

  (call(direct_child(man1, _G443)), call(a_descendant(_G443, grandkid)))) ?

  */

% without the CUT for matching 'true', upon backtracking we take 'true' and match it to the FINAL mini_solve predicate,
% which causes us to then call clause(true,B), which generates:
%    ERROR: clause/2: No permission to access private_procedure `true/0'
mini_solve(true) :- !.

% apparently (by observation, but i have not found documentation yet), after we already have done unification
% of everything in the explicit program during the 'clause(A,B)' execution, we ultimately unify with:
%   clause(ANYTHING,call(ANYTHING)) ... and then without this version of mini_solve we loop endlessly on that.
mini_solve(call(_)) :- !, fail.

% if problems occur, we should probably comment out the next clause and do a trace,
% because we might want to add a specific matcher, such as that for 'call' above.
mini_solve(A) :-
        predicate_property(A, autoload(_)), % using 'autoload' as a rough equivalent for built_in
        !,
        call(A).

% 'member' was not showing up as built-in, but its subclauses
% were. however, its subclauses were NON-visible, so could not be called with call ... ouch.
% / *  // despite the fact that built_in was no good on member, we need it for 'length'
mini_solve(A) :-
        predicate_property(A,built_in),
        !,
        call(A).
% * /

mini_solve((A,B)) :- mini_solve(A), mini_solve(B).

% why do i NOT need something like the following? i would like to explain this...
%  mini_solve((A;B)) :- mini_solve(A); mini_solve(B).

mini_solve(A) :- clause(A,B), mini_solve(B).

/*
NOTES ABOUT predicate_property AND POTENTIAL PROPERTIES FOR ITS SECOND ARG:

built_in
    True if the predicate is locked as a built-in predicate. This implies it cannot be redefined in its definition module and it can normally not be seen in the tracer.

foreign
    True if the predicate is defined in the C language.

interpreted
    True if the predicate is defined in Prolog. We return true on this because, although the code is actually compiled, it is completely transparent, just like interpreted code.
*/


