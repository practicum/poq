use_module(library(lists)).
use_module(library(assoc)).

hello_world :- write('Hello World!').

passes_test(red).
passes_test(green).


filterx([],[]).
filterx([X0|X1],[X0|Y])  :- passes_test(X0),   filterx(X1,Y).
filterx([X0|X1],Y)  :- \+passes_test(X0),   filterx(X1,Y).


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

/* for the current context, we define tuples that have the form:  a/b/x/u/n/p ... using the forward slash  */

crossx([],_L2,[]). /* A */
crossx(_L1,[],[]). /* B */
crossx([L1H|L1T],[L2H|L2T],OUT) :- crosswsingle(L1H,L1T,[L2H|L2T],[L2H|L2T],OUT). /* C ?????  */

crosswsingle(SNG,LA,[LB1H|LB1T],LB2,[SNG/LB1H|O1]) :- crosswsingle(SNG,LA,LB1T,LB2,O1). /* E */
crosswsingle(_SNG,LA,[],LB2,OUT) :- crossx(LA,LB2,OUT). /* D */

/* t is the empty mapping, from library assoc */
remove_duplicates(L,LOUT) :- rec_remove(L,t,LOUT).

rec_remove([],_ASSOC,[]).

rec_remove([LH|LT],MAP,OUT) :- get_assoc(LH,MAP,_EXISTSVAL), rec_remove(LT,MAP,OUT).

rec_remove([LH|LT],MAP,[LH|REST]) :- \+get_assoc(LH,MAP,_EXISTSVAL), put_assoc(LH,MAP,inmap,MAP2), rec_remove(LT,MAP2,REST).

/*
SELECT -star-
  FROM STUDENTS JOIN SCORES ON STUDENTS.STUDENT_NUM = SCORES.STUDENT_NUM

  WHERE SCORES.COURSE_ID = 10 AND SCORES.POINTS > 0

*/

is_x(x).

passes_test_two(_/_/VAL) :- is_x(VAL).
passes_test_three(A/_/FK/_) :- A =:= FK.



filter_two([],[]).
filter_two([X0|X1],[X0|Y])  :- passes_test_three(X0),   filter_two(X1,Y).
filter_two([X0|X1],Y)  :- \+passes_test_three(X0),   filter_two(X1,Y).


myselect(RA,RB,F1) :- crossx(RA,RB,RARB), filter_two(RARB,F1).