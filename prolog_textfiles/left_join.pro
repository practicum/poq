

:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

%:- use_module(modules/dbms/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

/*

C is ShoppingCart table.
CI is CartDetail table.
J is the intermediate join of C and CI (using the condition from the query)
Q_RESULT is the q_result


axiomatized_query(C,CI,J,Q_RESULT) :-

        join_on_expression(C,CI,J),
        group_by(J,t,LOUT),
        assoc_to_values(LOUT,Q_RESULT).


axiomatized_query(C,CI,J,Q_RESULT),
member( (CA,D1,CA,P), J ),
member( (CB,D2,CB,P), J),
member( (CA,D2,CA,P), Q_RESULT ),
CB \= CA,
D2 \= D1.




*/



person_tuple(
  PID) :-

        natural_type(PID), not_null(PID).

person_tuple_in_order(
  PID,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(PID,0,V0),
        RANK_OF_THIS_TUPLE is V0,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.


extra_info_tuple(
  PID,
  TITLE) :-

        natural_type(PID), not_null(PID),
        title_string_type(TITLE), not_null(TITLE).

extra_info_tuple_in_order(
  PID,
  TITLE,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(PID,0,V0),
        map_title(TITLE,1,V1),
        RANK_OF_THIS_TUPLE is V0 + V1,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.


% ----------------------------------------------------------


person_table(L) :-
        % t is the empty mapping, from library assoc
        person_table_with_constraints(L,t,_,L).


person_table_with_constraints([],_ASSOC,0,[]).


person_table_with_constraints(
  [(PID)   |LT],
  MAP,
  CURR_MAX,
  [(PID)   |REST]) :-

        within_table_size_limit([(PID)   |LT]),
        person_tuple(PID),

        \+get_assoc((PID),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((PID),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        person_table_with_constraints(LT,MAP2,LT_MAX,REST),
        person_tuple_in_order(PID,LT_MAX,CURR_MAX).


% ----------------------------------------------------------


extra_info_table(L) :-
        % t is the empty mapping, from library assoc
        extra_info_table_with_constraints(L,t,_,L).


extra_info_table_with_constraints([],_ASSOC,0,[]).


extra_info_table_with_constraints(
  [(PID,TITLE)   |LT],
  MAP,
  CURR_MAX,
  [(PID,TITLE)   |REST]) :-

        within_table_size_limit([(PID,TITLE)   |LT]),
        extra_info_tuple(PID,TITLE),

        \+get_assoc((PID,TITLE),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((PID,TITLE),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        extra_info_table_with_constraints(LT,MAP2,LT_MAX,REST),
        extra_info_tuple_in_order(PID,TITLE,LT_MAX,CURR_MAX).

