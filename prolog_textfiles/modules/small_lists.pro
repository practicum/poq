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
          [medium_list/1,
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



size_0_to_1(LIST) :- length(LIST,0),!;length(LIST,1).
size_0_to_2(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2).
size_0_to_3(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3).
size_0_to_4(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4).
size_0_to_5(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5).
size_0_to_6(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5);
                     length(LIST,6).
size_0_to_7(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5);
                     length(LIST,6),!;length(LIST,7).
size_0_to_8(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5);
                     length(LIST,6),!;length(LIST,7),!;length(LIST,8).
size_0_to_9(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5);
                     length(LIST,6),!;length(LIST,7),!;length(LIST,8),!;length(LIST,9).
size_0_to_10(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5);
                      length(LIST,6),!;length(LIST,7),!;length(LIST,8),!;length(LIST,9),!;length(LIST,10).
size_0_to_11(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5);
                      length(LIST,6),!;length(LIST,7),!;length(LIST,8),!;length(LIST,9),!;length(LIST,10),!;length(LIST,11).
size_0_to_12(LIST) :- length(LIST,0),!;length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5);
                      length(LIST,6),!;length(LIST,7),!;length(LIST,8),!;length(LIST,9),!;length(LIST,10),!;length(LIST,11),!;length(LIST,12).

size_1_to_2(LIST) :- length(LIST,1),!;length(LIST,2).
size_1_to_3(LIST) :- length(LIST,1),!;length(LIST,2),!;length(LIST,3).
size_1_to_4(LIST) :- length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4).
size_1_to_5(LIST) :- length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5).
size_1_to_6(LIST) :- length(LIST,1),!;length(LIST,2),!;length(LIST,3),!;length(LIST,4),!;length(LIST,5),!;length(LIST,6).


medium_list(LIST) :- length(LIST,6).
medium_list(LIST) :- length(LIST,10).
medium_list(LIST) :- length(LIST,3).
medium_list(LIST) :- length(LIST,0).
medium_list(LIST) :- length(LIST,34).
medium_list(LIST) :- length(LIST,47).
medium_list(LIST) :- length(LIST,78).
medium_list(LIST) :- length(LIST,95).
medium_list(LIST) :- length(LIST,75).
medium_list(LIST) :- length(LIST,82).
medium_list(LIST) :- length(LIST,61).
medium_list(LIST) :- length(LIST,4).
medium_list(LIST) :- length(LIST,63).
medium_list(LIST) :- length(LIST,13).
medium_list(LIST) :- length(LIST,67).
medium_list(LIST) :- length(LIST,46).
medium_list(LIST) :- length(LIST,20).
medium_list(LIST) :- length(LIST,54).
medium_list(LIST) :- length(LIST,52).
medium_list(LIST) :- length(LIST,42).
medium_list(LIST) :- length(LIST,38).
medium_list(LIST) :- length(LIST,15).
medium_list(LIST) :- length(LIST,97).
medium_list(LIST) :- length(LIST,91).
medium_list(LIST) :- length(LIST,16).
medium_list(LIST) :- length(LIST,83).
medium_list(LIST) :- length(LIST,50).
medium_list(LIST) :- length(LIST,9).
medium_list(LIST) :- length(LIST,65).
medium_list(LIST) :- length(LIST,81).
medium_list(LIST) :- length(LIST,29).
medium_list(LIST) :- length(LIST,17).
medium_list(LIST) :- length(LIST,8).
medium_list(LIST) :- length(LIST,18).
medium_list(LIST) :- length(LIST,25).
medium_list(LIST) :- length(LIST,79).
medium_list(LIST) :- length(LIST,49).
medium_list(LIST) :- length(LIST,70).
medium_list(LIST) :- length(LIST,26).
medium_list(LIST) :- length(LIST,77).
medium_list(LIST) :- length(LIST,35).
medium_list(LIST) :- length(LIST,44).
medium_list(LIST) :- length(LIST,89).
medium_list(LIST) :- length(LIST,32).
medium_list(LIST) :- length(LIST,62).
medium_list(LIST) :- length(LIST,14).
medium_list(LIST) :- length(LIST,99).
medium_list(LIST) :- length(LIST,27).
medium_list(LIST) :- length(LIST,19).
medium_list(LIST) :- length(LIST,43).
medium_list(LIST) :- length(LIST,60).
medium_list(LIST) :- length(LIST,86).
medium_list(LIST) :- length(LIST,51).
medium_list(LIST) :- length(LIST,30).
medium_list(LIST) :- length(LIST,55).
medium_list(LIST) :- length(LIST,100).
medium_list(LIST) :- length(LIST,33).
medium_list(LIST) :- length(LIST,40).
medium_list(LIST) :- length(LIST,59).
medium_list(LIST) :- length(LIST,98).
medium_list(LIST) :- length(LIST,56).
medium_list(LIST) :- length(LIST,11).
medium_list(LIST) :- length(LIST,94).
medium_list(LIST) :- length(LIST,88).
medium_list(LIST) :- length(LIST,68).
medium_list(LIST) :- length(LIST,96).
medium_list(LIST) :- length(LIST,28).
medium_list(LIST) :- length(LIST,12).
medium_list(LIST) :- length(LIST,93).
medium_list(LIST) :- length(LIST,58).
medium_list(LIST) :- length(LIST,73).
medium_list(LIST) :- length(LIST,66).
medium_list(LIST) :- length(LIST,72).
medium_list(LIST) :- length(LIST,90).
medium_list(LIST) :- length(LIST,41).
medium_list(LIST) :- length(LIST,69).
medium_list(LIST) :- length(LIST,85).
medium_list(LIST) :- length(LIST,76).
medium_list(LIST) :- length(LIST,37).
medium_list(LIST) :- length(LIST,87).
medium_list(LIST) :- length(LIST,48).
medium_list(LIST) :- length(LIST,7).
medium_list(LIST) :- length(LIST,84).
medium_list(LIST) :- length(LIST,80).
medium_list(LIST) :- length(LIST,5).
medium_list(LIST) :- length(LIST,1).
medium_list(LIST) :- length(LIST,45).
medium_list(LIST) :- length(LIST,2).
medium_list(LIST) :- length(LIST,22).
medium_list(LIST) :- length(LIST,21).
medium_list(LIST) :- length(LIST,23).
medium_list(LIST) :- length(LIST,53).
medium_list(LIST) :- length(LIST,74).
medium_list(LIST) :- length(LIST,92).