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

?- 1 is sin(pi/2).   % Fails! sin(pi/2) evaluates to the float 1.0, which does not unify with the integer 1.
?- 1 =:= sin(pi/2).  % Succeeds as expected.

*/

student(SID,NAME) :-
        demonat(SID),
        demoname(NAME).

scores(SID,CID,POINTS) :-
        demonat(SID),
        demoguid(CID),
        demoint(POINTS).

student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) :-
        demonat(SID1),
        demonat(SID2),
        demoname(NAME),
        demoguid(CID),
        demoint(POINTS).

primary_key_student_sid(L) :-
        unique_student_sid(L),
        \+has_null_student_sid(L).

has_null_student_sid(L) :-
        member(student(SID,NAME),L),
        isnull(SID),
        demoname(NAME).

unique_student_sid([]).

unique_student_sid([student(SID,NAME)|LT]) :-
        no_dupe_sid([student(SID,NAME)|LT],UX),
        [student(SID,NAME)|LT]=UX.

/* t is the empty mapping, from library assoc */
no_dupe_sid(L,LOUT) :-
        rec_remove_sid(L,t,LOUT).

rec_remove_sid([],_ASSOC,[]).

rec_remove_sid([student(LH_SID,NAME)|LT],MAP,OUT) :-
        size_0_to_6(LT),
        demonat(LH_SID),
        demoname(NAME),
        get_assoc(LH_SID,MAP,_EXISTSVAL),
        rec_remove_sid(LT,MAP,OUT).

rec_remove_sid([student(LH_SID,NAME)|LT],MAP,[student(LH_SID,NAME)|REST]) :-
        size_0_to_6(LT),
        demoname(NAME),
        demonat(LH_SID),
        \+get_assoc(LH_SID,MAP,_EXISTSVAL),
        put_assoc(LH_SID,MAP,inmap,MAP2),
        rec_remove_sid(LT,MAP2,REST).


crossx([],L2,[]) :-
        size_0_to_12(L2).

crossx(L1,[],[]) :-
        L1 \= [],
        size_0_to_12(L1).

crossx([L1H|L1T],[L2H|L2T],OUT) :-
        size_0_to_12(L1T),
        size_0_to_12(L2T),
        cross_loop(L1H,L1T,[L2H|L2T],[L2H|L2T],OUT).

cross_loop(_SNG,LA,[],LB2,OUT) :-
        crossx(LA,LB2,OUT).

cross_loop(
  student(SID1,NAME),
  LA,
  [scores(SID2,CID,POINTS)|LB1T],
  LB2,
  [student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS))|O1]) :-
        demonat(SID1),
        demonat(SID2),
        demoname(NAME),
        demoguid(CID),
        demoint(POINTS),
        cross_loop(student(SID1,NAME),LA,LB1T,LB2,O1).


is_good(2).

% TODO: need to handle NULL sid here! (not for when sid is a PK, but for the 'general case')
filter_for_join_on_sid( student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) ) :-
        demonat(SID1),
        demonat(SID2),
        demoname(NAME),
        demoguid(CID),
        demoint(POINTS),
        SID1 = SID2.

filter_on_is_good( student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) ) :-
        demonat(SID1),
        demonat(SID2),
        demoname(NAME),
        demoguid(CID),
        demoint(POINTS),
        is_good(POINTS).

join_on_sid([],[]).
join_on_sid([X0|X1],[X0|Y]) :-
        size_0_to_3(X1),
        filter_for_join_on_sid(X0),
        join_on_sid(X1,Y).

join_on_sid([X0|X1],Y) :-
        size_0_to_3(X1),
        \+filter_for_join_on_sid(X0),
        join_on_sid(X1,Y).

meets_cond_is_good([],[]).
meets_cond_is_good([X0|X1],[X0|Y]) :-
        size_0_to_2(X1),
        filter_on_is_good(X0),
        meets_cond_is_good(X1,Y).

meets_cond_is_good([X0|X1],Y) :-
        size_0_to_2(X1),
        \+filter_on_is_good(X0),
        meets_cond_is_good(X1,Y).


myselect(RA,RB,F2) :-
        primary_key_student_sid(RA),
        crossx(RA,RB,RARB),
        join_on_sid(RARB,F1),
        meets_cond_is_good(F1,F2).



/*
sample execution:

?- myselect(X,Y, Z ),length(Z,N),N>0.

?- length(X,3),length(Y,1),primary_key_student_sid(X),size_0_to_12(Z),myselect(X,Y,Z).

?- length(X,3),length(Y,1),length(Z,0),myselect(X,Y,Z).

?- length([student(a,k),student(b,l),student(c,m)],3),length([scores(a,sp,bad)],1),length(Z,0),myselect([student(a,k),student(b,l),student(c,m)],[scores(a,sp,bad)],Z).

*/
