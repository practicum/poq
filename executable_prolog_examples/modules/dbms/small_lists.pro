/*
A module file is a file holding a module/2 directive as its first
term. The module/2 directive declares the name and the public
(i.e., externally visible) predicates of the module.

Below is an example of a module file, defining reverse/2 and
hiding the helper predicate rev/3.

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
     [non_empty_table/1,
      within_table_size_limit/1,
      within_joined_size_limit/1]).


% note the use of SEMI-COLON, which is DISJUNCTION
within_table_size_limit(LIST) :-
        length(LIST,0);
        length(LIST,1);
        length(LIST,2);
        length(LIST,3);
        length(LIST,4);
        length(LIST,5);
        length(LIST,6).


% note the use of SEMI-COLON, which is DISJUNCTION
within_joined_size_limit(LIST) :-
        length(LIST,0);
        length(LIST,1);
        length(LIST,2);
        length(LIST,3);
        length(LIST,4);
        length(LIST,5);
        length(LIST,6);
        length(LIST,7);
        length(LIST,8);
        length(LIST,9);
        length(LIST,10);
        length(LIST,11);
        length(LIST,12);
        length(LIST,13);
        length(LIST,14);
        length(LIST,15);
        length(LIST,16);
        length(LIST,17);
        length(LIST,18);
        length(LIST,19);
        length(LIST,20);
        length(LIST,21);
        length(LIST,22);
        length(LIST,23);
        length(LIST,24);
        length(LIST,25).

% note the use of SEMI-COLON, which is DISJUNCTION
non_empty_table(LIST) :-
        length(LIST,1);
        length(LIST,2);
        length(LIST,3);
        length(LIST,4);
        length(LIST,5);
        length(LIST,6).

