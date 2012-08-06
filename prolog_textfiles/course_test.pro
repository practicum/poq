:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(modules/smallnum).
:- use_module(modules/datatypes).

:- write('        ~~~~ course_test file opened').


student(SID,NAME) :- small1(SID),
                     demoname(NAME).

scores(SID,CID,POINTS) :- small1(SID),
                          demoguid(CID),
                          demoint(POINTS).

student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) :-
                     small1(SID1),
                     small1(SID2),
                     demoname(NAME),
                     demoguid(CID),
                     demoint(POINTS).

/* t is the empty mapping, from library assoc */
no_dupe_sid(L,LOUT) :- rec_remove_sid(L,t,LOUT).

rec_remove_sid([],_ASSOC,[]).

rec_remove_sid([student(LH_SID,NAME)|LT],MAP,OUT) :-
        size_0_to_6(LT),small1(LH_SID),demoname(NAME),get_assoc(LH_SID,MAP,_EXISTSVAL), rec_remove_sid(LT,MAP,OUT).

rec_remove_sid([student(LH_SID,NAME)|LT],MAP,[student(LH_SID,NAME)|REST]) :-
        size_0_to_6(LT),demoname(NAME),small1(LH_SID),\+get_assoc(LH_SID,MAP,_EXISTSVAL), put_assoc(LH_SID,MAP,inmap,MAP2), rec_remove_sid(LT,MAP2,REST).




crossx([],L2,[]) :- size_0_to_12(L2).
crossx(L1,[],[]) :- L1 \= [], size_0_to_12(L1).
crossx([L1H|L1T],[L2H|L2T],OUT) :- size_0_to_12(L1T),size_0_to_12(L2T),cross_loop(L1H,L1T,[L2H|L2T],[L2H|L2T],OUT).

cross_loop(_SNG,LA,[],LB2,OUT) :- crossx(LA,LB2,OUT).

cross_loop(student(SID1,NAME),LA,[scores(SID2,CID,POINTS)|LB1T],LB2,[student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS))|O1]) :-
                     small1(SID1),
                     small1(SID2),
                     demoname(NAME),
                     demoguid(CID),
                     demoint(POINTS),
                     cross_loop(student(SID1,NAME),LA,LB1T,LB2,O1).

/*
SELECT -star-
  FROM STUDENTS JOIN SCORES ON STUDENTS.STUDENT_NUM = SCORES.STUDENT_NUM

  WHERE SCORES.COURSE_ID = 10 AND SCORES.POINTS > 0

*/

is_good(2).

/*
  Note: i tried changing to using '==' chosen from listing: http://www.lix.polytechnique.fr/~liberti/public/computing/prog/prolog/prolog-tutorial.html#expr , ... but it made prolog no longer able to 'solve' some of my queries...

 == means 'identical'. it is NOT TRUE that 3 == 4 - 1.

  ... i am still not clear why i cannot 'get away with' using == here.
*/
passes_test_two( student_x_scores(student(SID1,_NAME),scores(SID2,_CID,_POINTS)) ) :-
	small1(SID1), small1(SID1), SID1 = SID2.

passes_test_three( student_x_scores(student(SID1,_NAME),scores(SID2,_CID,POINTS)) ) :-
	small1(SID1), small1(SID2), is_good(POINTS).

filter_two([],[]).
filter_two([X0|X1],[X0|Y])  :- size_0_to_3(X1),passes_test_two(X0),   filter_two(X1,Y).
filter_two([X0|X1],Y)  :- size_0_to_3(X1), \+passes_test_two(X0),   filter_two(X1,Y).

filter_three([],[]).
filter_three([X0|X1],[X0|Y])  :- size_0_to_2(X1),passes_test_three(X0),   filter_three(X1,Y).
filter_three([X0|X1],Y)  :- size_0_to_2(X1),\+passes_test_three(X0),   filter_three(X1,Y).



myselect(RA,RB,F2) :- crossx(RA,RB,RARB), filter_two(RARB,F1), filter_three(F1,F2).



/*
sample execution:

?- myselect(X,Y, Z ),length(Z,N),N>0.
X = [student(_G470, _G471)],
Y = [scores(_G470, _G474, _G475)],
Z = [student_x_scores(student(_G470, _G471), scores(_G470, _G474, _G475))],
N = 1 .

?- length(X,3),length(Y,1),no_dupe_sid(X,PKX),X=PKX,size_0_to_12(Z),myselect(X,Y,Z).
X = [student(0, _G42), student(1, _G57), student(2, _G78)],
Y = [scores(0, _G129, good)],
PKX = [student(0, _G42), student(1, _G57), student(2, _G78)],
Z = [student_x_scores(student(0, _G42), scores(0, _G129, good))] .


% need to figure out why the first one is consistently 'false' although the second one (same thing, essentially) is satisfiable.

?- length(X,3),length(Y,1),length(Z,0),myselect(X,Y,Z).
  false.

?- length([student(a,k),student(b,l),student(c,m)],3),length([scores(a,sp,bad)],1),length(Z,0),myselect([student(a,k),student(b,l),student(c,m)],[scores(a,sp,bad)],Z).
Z = [] .


*/
