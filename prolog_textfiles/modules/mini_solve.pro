
:- module(mini_solve,
          [mini_solve/1,
           mini_solve_id/2]).


mini_solve(A) :-
        mini_solve(A,0,65535).% same call that the I.D. version makes, but with a BIG limit

% for the I.D. version, we first honor the user's inputted LIMIT
mini_solve_id(A,LIMIT) :-
        mini_solve(A,0,LIMIT).

% after honoring the user's inputted LIMIT (above), now we increase the limit and recurse.
mini_solve_id(A,LIMIT) :-
        write('limit='),
        nl,
        write(LIMIT),
%                            write('(Hit Enter to Continue.)'),
%                            get0(C),
%                            ( C == 10 ->
        NEXT_LIMIT is LIMIT + 1,
        mini_solve_id(A,NEXT_LIMIT)
%).
.

% ------------------------------------------------------------------------
% ------------------------------------------------------------------------

% without the CUT for matching 'true', upon backtracking we take 'true' and match it to the FINAL mini_solve predicate,
% which causes us to then call clause(true,B), which generates:
%    ERROR: clause/2: No permission to access private_procedure `true/0'
mini_solve(true,_D,_L) :- !.


% this clause for mini_solve was not added until we began doing ITERATIVE DEEPENING
mini_solve(_,Depth,Limit) :-
        Depth > Limit, % if this becomes true, then:
        !,             % forbid use of other 'mini_solve' clauses for further attempts, and
        fail.          % fail now


% apparently (by observation, but i have not found documentation yet), after we already have done unification
% of everything in the explicit program during the 'clause(A,B)' execution, we ultimately unify with:
%   clause(ANYTHING,call(ANYTHING)) ... and then without this version of mini_solve we loop endlessly on that.
mini_solve(call(_),_D,_L) :- !, fail.

% if problems occur, we should probably comment out the next clause and do a trace,
% because we might want to add a specific matcher, such as that for 'call' above.
mini_solve(A,_D,_L) :-
        predicate_property(A, autoload(_)), % using 'autoload' as a rough equivalent for built_in
        !,
        call(A).

% 'member' was not showing up as built-in, but its subclauses
% were. however, its subclauses were NON-visible, so could not be called with call ... ouch.
% / *  // despite the fact that built_in was no good on member, we need it for 'length'
mini_solve(A,_D,_L) :-
        predicate_property(A,built_in),
        !,
        call(A).
% * /

mini_solve((A,B),Depth,Limit) :-
        mini_solve(A,Depth,Limit),
        mini_solve(B,Depth,Limit).

% why do i NOT need something like the following? i would like to explain this...
%  mini_solve((A;B)) :- mini_solve(A); mini_solve(B).

mini_solve(A,Depth,Limit) :-
        clause(A,B),
        New_Depth is Depth + 1,
        mini_solve(B,New_Depth,Limit).

/*
NOTES ABOUT predicate_property AND POTENTIAL PROPERTIES FOR ITS SECOND ARG:

built_in
    True if the predicate is locked as a built-in predicate. This implies it cannot be redefined in its definition module and it can normally not be seen in the tracer.

foreign
    True if the predicate is defined in the C language.

interpreted
    True if the predicate is defined in Prolog. We return true on this because, although the code is actually compiled, it is completely transparent, just like interpreted code.
*/


/*
BECAUSE THE META-INTERPRETER IS SO IMPLEMENTATION-DEPENDENT,
  [currently dependent upon the behavior of linux source-built SWI-Prolog (Multi-threaded, 64 bits, Version 6.2.1)]
HERE ARE SOME CLAUSES THAT WILL BE USEFUL FOR TESTING THE CONTINUING PROPER BEHAVIOR OF THE META-INTERPRETER:

?- mini_solve(member(X,[a,b,c])).

?- mini_solve(taught_by( A, X)).

?- mini_solve(t_student_x_score(A,B)).

?- mini_solve(t_list_type_student(A)).

?- mini_solve(size_0_to_12(X)).

a_descendant(X,Y) :- direct_child(X,Y).
a_descendant(X,Z) :- direct_child(X,Y),a_descendant(Y,Z).

direct_child(granddad,man1).
direct_child(granddad,man2).
direct_child(man2,grandkid).

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

% / *
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

% * /



% / *
  Exit: (10) clause
  (  (direct_child(man1, _G443), a_descendant(_G443, grandkid)),

  (call(direct_child(man1, _G443)), call(a_descendant(_G443, grandkid)))) ?

% * /


siblings(a1,x1).
siblings(a2,x2).
siblings(a3,x3).

% DEMO of INFINITE tree
siblings(X,Y) :- siblings(Y,X).
% ?- siblings(X,Y) % produces a never-ending stream of assignments


*/