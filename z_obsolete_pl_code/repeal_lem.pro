


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

/*
an example that asks whether the sum of outputs of both queries can ever sum to LESS than the size of the source table.
  (and it shows that, presumably surprisingly and helpfully, the answer actually is YES).

remove_nonmatches([X|XT],XR),inverse_remove_nonmatches([X|XT],XR2),length([X|XT],LOT),length(XR,LXR),length(XR2,LXR2),SUM is LXR+LXR2,SUM@<LOT.
  */

t_Person(
  FIRST,
  MIDDLE,
  LAST) :-

        demoname(FIRST), nonnull(FIRST),
        demoname(MIDDLE),
        demoname(LAST), nonnull(LAST).



% ----------------------------------------------------------

% the uniqueness contraint (compound key) in this case is not really meant to reflect 'real life'
% it is just a reasonable restriction on the search space in this case.
t_table_content_person(L) :-
        % t is the empty mapping, from library assoc
        list_type_p_compound_key(L,t,L).


list_type_p_compound_key([],_ASSOC,[]).


list_type_p_compound_key(
  [p(F,M,L)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_Person(F,M,L),

        get_assoc(p(F,M,L),MAP,_EXISTSVAL), % map key (CART_sp) needs to be instantiated by here.

        list_type_p_compound_key(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_p_compound_key(
  [p(F,M,L)   |LT],
  MAP,
  [p(F,M,L)   |REST]) :-

        manageable_list_tail(LT),
        t_Person(F,M,L),

        \+get_assoc(p(F,M,L),MAP,_EXISTSVAL),  % map key (p(F,M,L)) needs to be instantiated by here.
        put_assoc(p(F,M,L),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_p_compound_key(LT,MAP2,REST).



meets_criteria_middle_name(p(_F,M,_L)) :-
        M = jacob.

inverse_criteria(p(_F,M,_L)) :-
        nonnull(M),
        M \= jacob.



remove_nonmatches([],[]).
remove_nonmatches([X0|X1],[X0|Y]) :-
        t_table_content_person([X0|X1]),
        meets_criteria_middle_name(X0),
        remove_nonmatches(X1,Y).

remove_nonmatches([X0|X1],Y) :-
        t_table_content_person([X0|X1]),
        \+meets_criteria_middle_name(X0),
        remove_nonmatches(X1,Y).



inverse_remove_nonmatches([],[]).
inverse_remove_nonmatches([X0|X1],[X0|Y]) :-
        t_table_content_person([X0|X1]),
        inverse_criteria(X0),
        inverse_remove_nonmatches(X1,Y).

inverse_remove_nonmatches([X0|X1],Y) :-
        t_table_content_person([X0|X1]),
        \+inverse_criteria(X0),
        inverse_remove_nonmatches(X1,Y).