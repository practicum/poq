use_module(library(lists)).
use_module(library(assoc)).

hello_world :- write('Hello World!').

mappingx(red,rojo).
mappingx(yellow,amarillo).
mappingx(blue,azul).

mapx([],[]).
mapx([X0|X1],[MX|R]) :-  mappingx(X0,MX),  mapx(X1,R).

redux(red,0).
redux(blue,2).
redux(yellow,4).

reducex([],0). /* :- integer(0).*/
reducex([X0|X1],N) :- reducex(X1,M), redux(X0,P),  N is M + P.


student(_SID,_NAME).
scores(_SID,_CID,_POINTS).

student_x_scores(student(_SID1,_NAME),scores(_SID2,_CID,_POINTS)). % :- student(SID1,NAME), scores(SID2,CID,POINTS).

/* t is the empty mapping, from library assoc */
no_dupe_sid(L,LOUT) :- rec_remove_sid(L,t,LOUT).

rec_remove_sid([],_ASSOC,[]).

rec_remove_sid([student(LH_SID,_NAME)|LT],MAP,OUT) :-
	get_assoc(LH_SID,MAP,_EXISTSVAL), rec_remove_sid(LT,MAP,OUT).

rec_remove_sid([student(LH_SID,NAME)|LT],MAP,[student(LH_SID,NAME)|REST]) :-
	\+get_assoc(LH_SID,MAP,_EXISTSVAL), put_assoc(LH_SID,MAP,inmap,MAP2), rec_remove_sid(LT,MAP2,REST).




crossx([],_L2,[]).
crossx(_L1,[],[]).
crossx([L1H|L1T],[L2H|L2T],OUT) :- crosswsingle(L1H,L1T,[L2H|L2T],[L2H|L2T],OUT).

crosswsingle(_SNG,LA,[],LB2,OUT) :- crossx(LA,LB2,OUT).
crosswsingle(SNG,LA,[LB1H|LB1T],LB2,[student_x_scores(SNG,LB1H)|O1]) :-
	crosswsingle(SNG,LA,LB1T,LB2,O1).

/* t is the empty mapping, from library assoc */
remove_duplicates(L,LOUT) :- rec_remove(L,t,LOUT).

rec_remove([],_ASSOC,[]).
rec_remove([LH|LT],MAP,OUT)       :- get_assoc(LH,MAP,_EXISTSVAL), rec_remove(LT,MAP,OUT).
rec_remove([LH|LT],MAP,[LH|REST]) :- \+get_assoc(LH,MAP,_EXISTSVAL), put_assoc(LH,MAP,inmap,MAP2), rec_remove(LT,MAP2,REST).

/*
SELECT -star-
  FROM STUDENTS JOIN SCORES ON STUDENTS.STUDENT_NUM = SCORES.STUDENT_NUM

  WHERE SCORES.COURSE_ID = 10 AND SCORES.POINTS > 0

*/

is_good(good).

passes_test_two( student_x_scores(student(SID1,_NAME),scores(SID2,_CID,_POINTS)) ) :-
	SID1 = SID2.

passes_test_three( student_x_scores(student(_SID1,_NAME),scores(_SID2,_CID,POINTS)) ) :-
	is_good(POINTS).

filter_two([],[]).
filter_two([X0|X1],[X0|Y])  :- passes_test_two(X0),   filter_two(X1,Y).
filter_two([X0|X1],Y)  :- \+passes_test_two(X0),   filter_two(X1,Y).

filter_three([],[]).
filter_three([X0|X1],[X0|Y])  :- passes_test_three(X0),   filter_three(X1,Y).
filter_three([X0|X1],Y)  :- \+passes_test_three(X0),   filter_three(X1,Y).



myselect(RA,RB,F2) :- crossx(RA,RB,RARB), filter_two(RARB,F1), filter_three(F1,F2).

/*
sample execution:

?- myselect(X,Y, Z ),length(Z,N),N>0.
X = [student(_G470, _G471)],
Y = [scores(_G470, _G474, _G475)],
Z = [student_x_scores(student(_G470, _G471), scores(_G470, _G474, _G475))],
N = 1 .

?- length(X,3),length(Y,1),no_dupe_sid(X,PKX),X=PKX,length(Z,N),N<5,myselect(X,Y,Z).
X = [student(_G1009, _G1010), student(_G1009, _G1025), student(_G1009, _G1064)],
Y = [scores(_G1009, _G1181, good)],
PKX = [student(_G1009, _G1010), student(_G1009, _G1025), student(_G1009, _G1064)],
Z = [student_x_scores(student(_G1009, _G1010), scores(_G1009, _G1181, good)), student_x_scores(student(_G1009, _G1025), scores(_G1009, _G1181, good)), student_x_scores(student(_G1009, _G1064), scores(_G1009, _G1181, good))],
N = 3 .


*/
