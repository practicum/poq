/*
A module file is a file holding a module/2 directive as its first
term. The module/2 directive declares the name and the public (i.e.,
externally visible) predicates of the module.

Below is an example of a module file, defining reverse/2 and hiding
the helper predicate rev/3.

A module can use all built-in predicates and, by default, cannot
redefine system predicates.

:- module(reverse, [reverse/2]).

reverse(List1, List2) :-
        rev(List1, [], List2).

rev([], List, List).
rev([Head|List1], List2, List3) :-
        rev(List1, [Head|List2], List3).
*/

:- module(smallnum,
          [small1/1,
           small2/1,
           size_0_to_1/1,
           size_0_to_2/1,
           size_0_to_3/1,
           size_0_to_4/1,
           size_0_to_5/1,
           size_0_to_6/1,
           size_0_to_7/1,
           size_0_to_8/1,
           size_0_to_9/1,
           size_0_to_10/1,
           size_0_to_11/1,
           size_0_to_12/1,
           size_1_to_2/1,
           size_1_to_3/1,
           size_1_to_4/1,
           size_1_to_5/1,
           size_1_to_6/1]).


small1(0).
small1(1).
small1(2).

small2(0).
small2(1).
small2(2).
small2(3).
small2(4).
small2(5).
small2(6).
small2(7).
small2(8).
small2(9).
small2(10).

size_0_to_1(LIST) :- length(LIST,0);length(LIST,1).
size_0_to_2(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2).
size_0_to_3(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3).
size_0_to_4(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4).
size_0_to_5(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5).
size_0_to_6(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);
                     length(LIST,6).
size_0_to_7(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);
                     length(LIST,6);length(LIST,7).
size_0_to_8(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);
                     length(LIST,6);length(LIST,7);length(LIST,8).
size_0_to_9(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);
                     length(LIST,6);length(LIST,7);length(LIST,8);length(LIST,9).
size_0_to_10(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);
                      length(LIST,6);length(LIST,7);length(LIST,8);length(LIST,9);length(LIST,10).
size_0_to_11(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);
                      length(LIST,6);length(LIST,7);length(LIST,8);length(LIST,9);length(LIST,10);length(LIST,11).
size_0_to_12(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);
                      length(LIST,6);length(LIST,7);length(LIST,8);length(LIST,9);length(LIST,10);length(LIST,11);length(LIST,12).

size_1_to_2(LIST) :- length(LIST,1);length(LIST,2).
size_1_to_3(LIST) :- length(LIST,1);length(LIST,2);length(LIST,3).
size_1_to_4(LIST) :- length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4).
size_1_to_5(LIST) :- length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5).
size_1_to_6(LIST) :- length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5);length(LIST,6).



