/*
  https://github.com/practicum/poq

  Axiomatized query from example 1 of Chapter 5.
*/

:- use_module(modules/dbms/small_lists).


person_tuple(
  FIRST,
  MIDDLE,
  LAST) :-

        name_string_type(FIRST), not_null(FIRST),
        name_string_type(MIDDLE),
        name_string_type(LAST), not_null(LAST).


person_tuple_in_order(
  FIRST,
  MIDDLE,
  LAST,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_name(FIRST,0,V0),
        map_name(MIDDLE,1,V1),
        map_name(LAST,2,V2),
        RANK_OF_THIS_TUPLE is V0 + V1 + V2,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

% ----------------------------------------------------------

person_table(L) :-
        % t is the empty mapping, from library assoc
        person_table_with_constraints(L,t,_,L).

person_table_with_constraints([],_ASSOC,0,[]).

% Note: 'LT' stands for 'list tail'
person_table_with_constraints(
  [ (F,M,L)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (F,M,L)  |LT2]
  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (F,M,L)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        person_tuple(F,M,L),

        %negation on next line means key is not yet in map
        \+get_assoc((F,M,L),MAP,_EXISTSVAL),
        put_assoc((F,M,L),MAP,inmap,MAP2),
        person_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        person_tuple_in_order(F,M,L,LT_MAX,MAX).

% ----------------------------------------------------------


meets_criteria_middle_name(_F,M,_L) :-
        M = jacob.

remove_nonmatches([],[]).

remove_nonmatches([(F,M,L)|X1],[(F,M,L)|Y]) :-
        person_table([(F,M,L)|X1]),
        meets_criteria_middle_name(F,M,L),
        remove_nonmatches(X1,Y).

remove_nonmatches([(F,M,L)|X1],Y) :-
        person_table([(F,M,L)|X1]),
        \+meets_criteria_middle_name(F,M,L),
        remove_nonmatches(X1,Y).

% ----------------------------------------------------------

inverse_criteria(_F,M,_L) :-
        not_null(M),
        M \= jacob.

inverse_remove_nonmatches([],[]).

inverse_remove_nonmatches([(F,M,L)|X1],[(F,M,L)|Y]) :-
        person_table([(F,M,L)|X1]),
        inverse_criteria(F,M,L),
        inverse_remove_nonmatches(X1,Y).

inverse_remove_nonmatches([(F,M,L)|X1],Y) :-
        person_table([(F,M,L)|X1]),
        \+inverse_criteria(F,M,L),
        inverse_remove_nonmatches(X1,Y).

% ----------------------------------------------------------

axiomatized_query(Person,Q_RESULT) :-
        remove_nonmatches(Person,DT1),
        inverse_remove_nonmatches(Person,DT2),
        merge(DT1,DT2,Q_RESULT).

