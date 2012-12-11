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
          [% xxx_manageable_list_tail/1,
           within_table_size_limit/1,
           size_0_to_0/1, % yes, the name here is a bit silly, but i like the symmetry with the other names
           size_0_to_1/1,
           size_0_to_2/1,
           size_0_to_3/1,
           size_0_to_4/1,
           size_0_to_5/1]).



within_table_size_limit(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3).

% xxx_manageable_list_tail has to range from ZERO to something, with no 'gaps', otherwise user-code fails
xxx_manageable_list_tail(L) :- size_0_to_1(L). % applied to a TAIL of list, we know the WHOLE list would be +1 bigger
%xxx_manageable_list_tail(L) :- length(L,4);length(L,3);length(L,2);length(L,1);length(L,0). % sometimes it is helpful to reverse the order of the permissible lengths


size_0_to_0(LIST) :- length(LIST,0).
size_0_to_1(LIST) :- length(LIST,0);length(LIST,1).
size_0_to_2(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2).
size_0_to_3(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3).
size_0_to_4(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4).
size_0_to_5(LIST) :- length(LIST,0);length(LIST,1);length(LIST,2);length(LIST,3);length(LIST,4);length(LIST,5).
