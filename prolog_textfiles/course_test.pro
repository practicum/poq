:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(modules/small_lists).
:- use_module(modules/datatypes).

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
student(SID,NAME) :-
        demonat(SID),
        demoname(NAME).

% type definition for a scores tuple
scores(SID,CID,POINTS) :-
        demonat(SID),
        demoguid(CID),
        demoint(POINTS).

% type definition for a tuple from crossing student(s) with score(s)
student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) :-
        student(SID1,NAME),
        scores(SID2,CID,POINTS).


list_type_student([]).
list_type_student([student(SID,NAME)|LT]) :-
        student(SID,NAME),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        list_type_student(LT).

list_type_scores([]).
list_type_scores([scores(SID2,CID,POINTS)|LT]) :-
        scores(SID2,CID,POINTS),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        list_type_scores(LT).

list_type_student_x_scores([]).
list_type_student_x_scores([student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS))|LT]) :-
        student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        list_type_student_x_scores(LT).


list_type_student_2([]).
list_type_student_2([student(SID,NAME)|LT]) :-
        student(SID,NAME),
        nonnull(SID),
        size_0_to_12(LT),      % it is very important to put this size PRIOR to the recursion below
        list_type_student_2(LT).

primary_key_student_sid(L) :-
        list_type_student_2(L),
        unique_student_sid(L).


unique_student_sid([]).

unique_student_sid([student(SID,NAME)|LT]) :-
        list_type_student([student(SID,NAME)|LT]),
        no_dupe_sid([student(SID,NAME)|LT],UX),
        [student(SID,NAME)|LT]=UX.

/* t is the empty mapping, from library assoc */
no_dupe_sid(L,LOUT) :-
        list_type_student(L),
        %list_type_student(LOUT), % not yet sure whether type assertions on the output is good or bad
        rec_remove_sid(L,t,LOUT).

rec_remove_sid([],_ASSOC,[]).

rec_remove_sid([student(LH_SID,NAME)|LT],MAP,OUT) :-
        list_type_student([student(LH_SID,NAME)|LT]),
        %list_type_student(OUT), % not yet sure whether type assertions on the output is good or bad
        size_0_to_6(LT),
        get_assoc(LH_SID,MAP,_EXISTSVAL),
        rec_remove_sid(LT,MAP,OUT).

rec_remove_sid([student(LH_SID,NAME)|LT],MAP,[student(LH_SID,NAME)|REST]) :-
        list_type_student([student(LH_SID,NAME)|LT]),
        %list_type_student([student(LH_SID,NAME)|REST]), % not yet sure whether type assertions on the output is good or bad
        size_0_to_6(LT),
        \+get_assoc(LH_SID,MAP,_EXISTSVAL),
        put_assoc(LH_SID,MAP,inmap,MAP2),
        rec_remove_sid(LT,MAP2,REST).


aax(x).
bbx(x).
ccx(x).
ddx(x).

cross_students_scores( [], [], [] ).

cross_students_scores( [student(SID1,NAME)|[]], [], [] ) :-
        student(SID1,NAME).

cross_students_scores( [], [scores(SID2,CID,POINTS)|[]], [] ) :-
        scores(SID2,CID,POINTS).

% single student but longer list of scores
cross_students_scores(
    [student(SID1,NAME)|[]],
    [scores(SID2,CID,POINTS)|L2T],
    [student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS))|R]  ) :-

        aax(_),
        student(SID1,NAME),
        list_type_scores([scores(SID2,CID,POINTS)|L2T]),
        size_0_to_12(L2T),
        cross_students_scores( [student(SID1,NAME)|[]], L2T, R ).

% longer students list but SINGLE score
cross_students_scores(
    [student(SID1,NAME)|L2T],
    [scores(SID2,CID,POINTS)|[]],
    [student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS))|R]  ) :-

        bbx(_),
        list_type_student([student(SID1,NAME)|L2T]),
        scores(SID2,CID,POINTS),
        size_0_to_12(L2T),
        cross_students_scores( L2T, [scores(SID2,CID,POINTS)|[]], R ).

% adding one more score to an 'already crossing'
cross_students_scores(
    [student(_A,_B)|_C], % this list needs to be nonempty. the empty case is below.
    [scores(SID2,CID,POINTS)|L2T],
    FINAL ) :-

        ccx(_),
        list_type_student([student(_A,_B)|_C]),
        list_type_scores([scores(SID2,CID,POINTS)|L2T]),
        length([student(_A,_B)|_C],X),
        X>1,
        length([scores(SID2,CID,POINTS)|L2T],Y),
        Y>1,
        cross_students_scores([student(_A,_B)|_C],L2T,POUT),
        cross_students_scores([student(_A,_B)|_C],[scores(SID2,CID,POINTS)|[]],MOUT),
        merge(POUT,MOUT,FINAL).

% adding one more student to an 'already crossing'
cross_students_scores(
    [student(SID1,NAME)|L1T],
    [scores(_A,_B,_C)|_D], % this list needs to be nonempty. the empty case is below.
    FINAL ) :-

        ddx(_),
        list_type_student([student(SID1,NAME)|L1T]),
        list_type_scores([scores(_A,_B,_C)|_D]),
        length([student(SID1,NAME)|L1T],X),
        X>1,
        length([scores(_A,_B,_C)|_D],Y),
        Y>1,
        cross_students_scores(L1T,[scores(_A,_B,_C)|_D],POUT),
        cross_students_scores([student(SID1,NAME)|[]],[scores(_A,_B,_C)|_D],MOUT),
        merge(POUT,MOUT,FINAL).




% cross_students_scores([],L2,[]) :-
%         size_0_to_6(L2),
%         list_type_scores(L2).

% cross_students_scores(L1,[],[]) :-
%         %L1 \= [],
%         size_0_to_6(L1),
%         list_type_student(L1).


is_good(2).

% TODO: need to handle NULL sid here. (not for when sid is a PK, but for the 'general case')
filter_for_join_on_sid( student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) ) :-
        student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)),
        SID1 = SID2.

filter_on_is_good( student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) ) :-
        student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)),
        is_good(POINTS).

join_on_sid([],[]).
join_on_sid([X0|X1],[X0|Y]) :-
        list_type_student_x_scores([X0|X1]),
        size_0_to_3(X1),
        filter_for_join_on_sid(X0),
        join_on_sid(X1,Y).

join_on_sid([X0|X1],Y) :-
        list_type_student_x_scores([X0|X1]),
        size_0_to_3(X1),
        \+filter_for_join_on_sid(X0),
        join_on_sid(X1,Y).

meets_cond_is_good([],[]).
meets_cond_is_good([X0|X1],[X0|Y]) :-
        list_type_student_x_scores([X0|X1]),
        size_0_to_2(X1),
        filter_on_is_good(X0),
        meets_cond_is_good(X1,Y).

meets_cond_is_good([X0|X1],Y) :-
        list_type_student_x_scores([X0|X1]),
        size_0_to_2(X1),
        \+filter_on_is_good(X0),
        meets_cond_is_good(X1,Y).


myselect(RA,RB,F2) :-
        %list_type_student(RA),
        %list_type_scores(RB),
        primary_key_student_sid(RA),
        cross_students_scores(RA,RB,RARB),
        join_on_sid(RARB,F2).
        %meets_cond_is_good(F1,F2).



/*
sample execution:

?- myselect(X,Y, Z ),length(Z,N),N>0.

?- length(X,3),length(Y,1),primary_key_student_sid(X),size_0_to_12(Z),myselect(X,Y,Z).

?- length(X,3),length(Y,1),length(Z,0),myselect(X,Y,Z).

?- length([student(a,k),student(b,l),student(c,m)],3),length([scores(a,sp,bad)],1),length(Z,0),myselect([student(a,k),student(b,l),student(c,m)],[scores(a,sp,bad)],Z).

*/
