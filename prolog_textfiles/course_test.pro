:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(modules/small_lists).
:- use_module(modules/datatypes).
:- use_module(modules/mini_solve).

:- write('        ~~~~ course_test file opened').

% cuts are valid here (i believe) because once something matches we KNOW nothing else would ever succeed.
mapping_sample(red,rojo) :- !.
mapping_sample(yellow,amarillo) :- !.
mapping_sample(blue,azul) :- !.

% cut is valid here (i believe) because once we match on two empties, we KNOW nothing else would ever succeed.
do_map([],[]) :- !.
do_map([X0|X1],[MX|R]) :-  mapping_sample(X0,MX),  do_map(X1,R).

% not yet sure about WHY, but putting cuts here prevented this from working: do_reduce(A,6).
color_value(red,0).
color_value(blue,2).
color_value(yellow,4).

do_reduce([],0).
do_reduce([X0|X1],N) :- do_reduce(X1,M), color_value(X0,P),  N is M + P.

/*
% plain single '=' symbol is for UNIFICATION, and so makes no sense here:
?- 3 = 4-1.
false.

% the double '==' tests for IDENTICAL-ness of two atoms or two variables. also makes no sense here:
?- 3 == 4-1.
false.

% the use of 'is' causes the expression to be evaluated
?- 3 is 4 - 1.
true.

% also does evaluation and tests for equality (very similar to above).
?- 3 =:= 4-1.
true.

?- X =:= 4-1.
ERROR: =:=/2: Arguments are not sufficiently instantiated
?- 4-1 =:= X.
ERROR: =:=/2: Arguments are not sufficiently instantiated

% further information about the difference between 'is' and '=:='

Typically, is/2 should be used with unbound left operand. If equality is to be tested, =:=/2 should be used. For example:

?- 1 is sin(pi/2).   % Fails. sin(pi/2) evaluates to the float 1.0, which does not unify with the integer 1.
?- 1 =:= sin(pi/2).  % Succeeds as expected.

*/

% type definition for a student tuple
t_student(SID,NAME) :-
        demonat(SID),
        demoname(NAME).

% type definition for a score tuple
t_score(SID,CID,POINTS) :-
        demonat(SID),
        demoguid(CID),
        demoint(POINTS).

% type definition for a tuple from crossing student(s) with score(s)
t_student_x_score(student(SID1,NAME),score(SID2,CID,POINTS)) :-
        t_student(SID1,NAME),
        t_score(SID2,CID,POINTS).


t_list_type_student([]).
t_list_type_student([student(SID,NAME)|LT]) :-
        t_student(SID,NAME),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        t_list_type_student(LT).

t_list_type_score([]).
t_list_type_score([score(SID2,CID,POINTS)|LT]) :-
        t_score(SID2,CID,POINTS),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        t_list_type_score(LT).

t_list_type_student_x_score([]).
t_list_type_student_x_score([student_x_score(student(SID1,NAME),score(SID2,CID,POINTS))|LT]) :-
        t_student_x_score(student(SID1,NAME),score(SID2,CID,POINTS)),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        t_list_type_student_x_score(LT).


t_list_type_student_nonnull_sid([]).
t_list_type_student_nonnull_sid([student(SID,NAME)|LT]) :-
        t_student(SID,NAME),
        nonnull(SID),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        t_list_type_student_nonnull_sid(LT).

t_primary_key_student_sid(L) :-
        t_list_type_student_nonnull_sid(L),
        unique_student_sid(L).


unique_student_sid([]).

unique_student_sid([student(SID,NAME)|LT]) :-
        %t_list_type_student([student(SID,NAME)|LT]),
        no_dupe_sid([student(SID,NAME)|LT],UX),
        [student(SID,NAME)|LT]=UX.

/* t is the empty mapping, from library assoc */
no_dupe_sid(L,LOUT) :-
        %t_list_type_student(L),
        %t_list_type_student(LOUT), % not yet sure whether type assertions on the output is good or bad
        t_rec_remove_sid(L,t,LOUT).

t_rec_remove_sid([],_ASSOC,[]).

t_rec_remove_sid([student(LH_SID,NAME)|LT],MAP,OUT) :-
        t_list_type_student([student(LH_SID,NAME)|LT]),
        %t_list_type_student(OUT), % not yet sure whether type assertions on the output is good or bad
        size_0_to_6(LT),
        get_assoc(LH_SID,MAP,_EXISTSVAL),
        t_rec_remove_sid(LT,MAP,OUT).

t_rec_remove_sid([student(LH_SID,NAME)|LT],MAP,[student(LH_SID,NAME)|REST]) :-
        t_list_type_student([student(LH_SID,NAME)|LT]),
        %t_list_type_student([student(LH_SID,NAME)|REST]), % not yet sure whether type assertions on the output is good or bad
        size_0_to_6(LT),
        \+get_assoc(LH_SID,MAP,_EXISTSVAL),
        put_assoc(LH_SID,MAP,inmap,MAP2),
        t_rec_remove_sid(LT,MAP2,REST).


/*
There are 7 different clauses to express cross_student_score.

There should be no duplication in outcomes due to careful management
of when each of the 7 clauses is allowed to be applied.

Each one of the 7 handles a NON-OVERLAPPING subset of cases based on
the SIZE of the first two list variables.

The cases (by size of the two lists) are:

[]    []
1     []
[]    1
1     >1
1+    1     (1+ means 'one or more')
2+    2+  ... and the first list size is greater to or EQUAL to the second
2+    2+  ... and the first list size is LESS THAN the second
*/

cross_student_score( [], [], [] ).

cross_student_score( [student(SID1,NAME)|[]], [], [] ) :-
        t_student(SID1,NAME).

cross_student_score( [], [score(SID2,CID,POINTS)|[]], [] ) :-
        t_score(SID2,CID,POINTS).

% single student but longer list of score
cross_student_score(
    [student(SID1,NAME)|[]],
    [score(SID2,CID,POINTS)|L2T],
    [student_x_score(student(SID1,NAME),score(SID2,CID,POINTS))|R]  ) :-

        t_student(SID1,NAME),
        t_list_type_score([score(SID2,CID,POINTS)|L2T]),
        length([score(SID2,CID,POINTS)|L2T],X),
        X>1,
        size_0_to_12(L2T),
        cross_student_score( [student(SID1,NAME)|[]], L2T, R ).

% longer students list but SINGLE score
cross_student_score(
    [student(SID1,NAME)|L2T],
    [score(SID2,CID,POINTS)|[]],
    [student_x_score(student(SID1,NAME),score(SID2,CID,POINTS))|R]  ) :-

        t_list_type_student([student(SID1,NAME)|L2T]),
        t_score(SID2,CID,POINTS),
        size_0_to_12(L2T),
        cross_student_score( L2T, [score(SID2,CID,POINTS)|[]], R ).

% adding one more score to an 'already crossing'
cross_student_score(
    [student(A,B)|C], % this list needs to be nonempty. the empty case is handled elsewhere
    [score(SID2,CID,POINTS)|L2T],
    FINAL ) :-

        t_list_type_student([student(A,B)|C]),
        t_list_type_score([score(SID2,CID,POINTS)|L2T]),
        length([student(A,B)|C],X),
        X>1,
        length([score(SID2,CID,POINTS)|L2T],Y),
        Y>1,
        X>=Y,
        cross_student_score([student(A,B)|C],L2T,POUT),
        cross_student_score([student(A,B)|C],[score(SID2,CID,POINTS)|[]],MOUT),
        merge(POUT,MOUT,FINAL).

% adding one more student to an 'already crossing'
cross_student_score(
    [student(SID1,NAME)|L1T],
    [score(A,B,C)|D], % this list needs to be nonempty. the empty case is handled elsewhere
    FINAL ) :-

        t_list_type_student([student(SID1,NAME)|L1T]),
        t_list_type_score([score(A,B,C)|D]),
        length([student(SID1,NAME)|L1T],X),
        X>1,
        length([score(A,B,C)|D],Y),
        Y>1,
        X<Y,
        cross_student_score(L1T,[score(A,B,C)|D],POUT),
        cross_student_score([student(SID1,NAME)|[]],[score(A,B,C)|D],MOUT),
        merge(POUT,MOUT,FINAL).


is_good(2).

% TODO: need to handle NULL sid here. (not for when sid is a PK, but for the 'general case')
filter_for_join_on_sid( student_x_score(student(SID1,NAME),score(SID2,CID,POINTS)) ) :-
        t_student_x_score(student(SID1,NAME),score(SID2,CID,POINTS)),
        SID1 = SID2.

filter_on_is_good( student_x_score(student(SID1,NAME),score(SID2,CID,POINTS)) ) :-
        t_student_x_score(student(SID1,NAME),score(SID2,CID,POINTS)),
        is_good(POINTS).

join_on_sid([],[]).
join_on_sid([X0|X1],[X0|Y]) :-
        t_list_type_student_x_score([X0|X1]),
        size_0_to_3(X1),
        filter_for_join_on_sid(X0),
        join_on_sid(X1,Y).

join_on_sid([X0|X1],Y) :-
        t_list_type_student_x_score([X0|X1]),
        size_0_to_3(X1),
        \+filter_for_join_on_sid(X0),
        join_on_sid(X1,Y).

meets_cond_is_good([],[]).
meets_cond_is_good([X0|X1],[X0|Y]) :-
        t_list_type_student_x_score([X0|X1]),
        size_0_to_2(X1),
        filter_on_is_good(X0),
        meets_cond_is_good(X1,Y).

meets_cond_is_good([X0|X1],Y) :-
        t_list_type_student_x_score([X0|X1]),
        size_0_to_2(X1),
        \+filter_on_is_good(X0),
        meets_cond_is_good(X1,Y).


myselect(RA,RB,F2) :-
        t_primary_key_student_sid(RA),
        cross_student_score(RA,RB,RARB),
        join_on_sid(RARB,F2).
        %meets_cond_is_good(F1,F2).



/*
sample execution:

?- myselect(X,Y, Z ),length(Z,N),N>0.

?- length(X,3),length(Y,1),t_primary_key_student_sid(X),size_0_to_12(Z),myselect(X,Y,Z).

?- length(X,3),length(Y,1),length(Z,0),myselect(X,Y,Z).

?- length([student(a,k),student(b,l),student(c,m)],3),length([score(a,sp,bad)],1),length(Z,0),myselect([student(a,k),student(b,l),student(c,m)],[score(a,sp,bad)],Z).

*/
