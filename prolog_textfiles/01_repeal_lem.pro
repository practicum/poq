


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/small_lists).

/*
an example that asks whether the sum of outputs of both queries can ever sum to LESS than the size of the source table.
  (and it shows that, presumably surprisingly and helpfully, the answer actually is YES).



axiomatized_query(Person,Q_RESULT),
length(Person,PLength),
length(Q_RESULT,QLength),
QLength@<PLength.

CEX:
Person = [ (william, jacob, isabella), (jacob, null, jacob)],
Q_RESULT = [ (william, jacob, isabella)],
PLength = 2,
QLength = 1 ;


  */

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

% the uniqueness contraint (compound key) in this case is not really meant to reflect 'real life'
% it is just a reasonable restriction on the search space in this case.
person_table(L) :-
        % t is the empty mapping, from library assoc
        person_table_with_constraints(L,t,_,L).


person_table_with_constraints([],_ASSOC,0,[]).


person_table_with_constraints(
  [(F,M,L)   |LT],
  MAP,
  CURR_MAX,
  [(F,M,L)   |REST]) :-

        within_table_size_limit([(F,M,L)   |LT]),
        person_tuple(F,M,L),

        \+get_assoc((F,M,L),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((F,M,L),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        person_table_with_constraints(LT,MAP2,LT_MAX,REST),
        person_tuple_in_order(F,M,L,LT_MAX,CURR_MAX).

% ----------------------------------------------------------


meets_criteria_middle_name(_F,M,_L) :-
        M = jacob.

inverse_criteria(_F,M,_L) :-
        not_null(M),
        M \= jacob.



remove_nonmatches([],[]).
remove_nonmatches([(F,M,L)|X1],[(F,M,L)|Y]) :-
        person_table([(F,M,L)|X1]),
        meets_criteria_middle_name(F,M,L),
        remove_nonmatches(X1,Y).

remove_nonmatches([(F,M,L)|X1],Y) :-
        person_table([(F,M,L)|X1]),
        \+meets_criteria_middle_name(F,M,L),
        remove_nonmatches(X1,Y).



inverse_remove_nonmatches([],[]).
inverse_remove_nonmatches([(F,M,L)|X1],[(F,M,L)|Y]) :-
        person_table([(F,M,L)|X1]),
        inverse_criteria(F,M,L),
        inverse_remove_nonmatches(X1,Y).

inverse_remove_nonmatches([(F,M,L)|X1],Y) :-
        person_table([(F,M,L)|X1]),
        \+inverse_criteria(F,M,L),
        inverse_remove_nonmatches(X1,Y).



axiomatized_query(Person,Q_RESULT) :-
        remove_nonmatches(Person,DT1),
        inverse_remove_nonmatches(Person,DT2),
        merge(DT1,DT2,Q_RESULT).

