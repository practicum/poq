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


passes_test_two( student_x_scores(student(SID1,_NAME),scores(SID2,_CID,_POINTS)) ) :-
	SID1 = SID2.


filter_two([],[]).
filter_two([X0|X1],[X0|Y])  :- passes_test_two(X0),   filter_two(X1,Y).
filter_two([X0|X1],Y)  :- \+passes_test_two(X0),   filter_two(X1,Y).


myselect(RA,RB,F1) :- crossx(RA,RB,RARB), filter_two(RARB,F1).

/*
sample execution:

?- myselect(X,Y, Z ),length(Z,N),N>0.
X = [student(_G470, _G471)],
Y = [scores(_G470, _G474, _G475)],
Z = [student_x_scores(student(_G470, _G471), scores(_G470, _G474, _G475))],
N = 1 .


*/
