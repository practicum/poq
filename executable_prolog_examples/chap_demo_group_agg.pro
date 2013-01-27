/*
  File: chapter_text.pro

  */


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).




% =====================================================================


table_two_tuple(
  ONE_ID,
  PSTR) :-

        natural_type(ONE_ID), not_null(ONE_ID),
        product_string_type(PSTR), not_null(PSTR).


table_two_tuple_in_order(
  ONE_ID,
  PSTR,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(ONE_ID,0,V0),
        map_product(PSTR,1,V1),
        RANK_OF_THIS_TUPLE is V0 + V1,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

% ----------------------------------------------------------

table_two_table(L) :-
        % t is the empty mapping, from library assoc
        table_two_table_with_constraints(L,t,_,L).


table_two_table_with_constraints([],_ASSOC,0,[]).


table_two_table_with_constraints(
  [(ONE_ID, PSTR)   |LT],
  MAP,
  CURR_MAX,
  [(ONE_ID, PSTR)   |REST]) :-

        within_table_size_limit([(ONE_ID, PSTR)   |LT]),
        table_two_tuple(ONE_ID, PSTR),

        \+get_assoc((ONE_ID, PSTR),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((ONE_ID, PSTR),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        table_two_table_with_constraints(LT,MAP2,LT_MAX,REST),
        table_two_tuple_in_order(ONE_ID, PSTR,LT_MAX,CURR_MAX).


% ----------------------------------------------------------



/*
+-------+-----------+
| oneid | pstr      |
+-------+-----------+
|     1 | aspirin   |
|     2 | aspirin   |
|     0 | aspirin   |
|     1 | ibuprofen |
+-------+-----------+

this is the 'group by' being processed:

mysql> select max(oneid), pstr from t2 group by pstr;
+------------+-----------+
| max(oneid) | pstr      |
+------------+-----------+
|          2 | aspirin   |
|          1 | ibuprofen |
+------------+-----------+
2 rows in set (0.01 sec)


this is the aggregation with EMPTY group-by clause:

mysql> select max(oneid), pstr from t2;
+------------+---------+
| max(oneid) | pstr    |
+------------+---------+
|          2 | aspirin |
+------------+---------+
1 row in set (0.00 sec)


*/


% ----------------------------------------------------------



required_tuple_type_for_group_by(COL_1,COL_2) :-
        table_two_tuple(COL_1,COL_2).

required_table_type_for_group_by(L) :-
        table_two_table(L).

restrict_list_tail_size(L) :-
        within_table_size_limit(L).

group_by_1(L,LOUT) :-

        required_table_type_for_group_by(L), % assert the type of the table
        group_by_1(L,t,LOUT).


% nothing in the list for further processing. so your 'map so-far' is your finished map.
group_by_1([],MAP,MAP) :-

        write( '   -----------------------   ' ), nl.


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
% in this case, the current row does group together with some already mapped key.
group_by_1(
  [(COL_1,COL_2)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2),

        get_assoc((COL_2), % map key needs to be instantiated by here.
                  MAP,
                  (COL_1_SOFAR,COL_2_SOFAR)),

        % there should be 1 line of 'agg_field' statement for each column in the table
        agg_field_max_atom(COL_1_SOFAR,COL_1,COL_1_AGG),
        agg_field_do_nothing(COL_2_SOFAR,COL_2,COL_2_AGG),

        put_assoc((COL_2),
                  MAP,
                  (COL_1_AGG,COL_2_AGG),
                  MAP2),

        group_by_1(LT,MAP2,MAP_OUT).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
% in this case, the group-key for the current row is new (never-seen so far), so we put starting values in the map for this key.
group_by_1(
  [(COL_1,COL_2)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2),

        \+get_assoc((COL_2),MAP,_), % map key needs to be instantiated by here.

        % there should be 1 line of 'agg_base' statement for each column in the table
        agg_base_max_atom(COL_1,COL_1_AGG),
        agg_base_do_nothing(COL_2,COL_2_AGG),

        put_assoc((COL_2),
                  MAP,
                  (COL_1_AGG,COL_2_AGG),
                  MAP2),
        group_by_1(LT,MAP2,MAP_OUT).






empty_group_by(L,LOUT) :-

        required_table_type_for_group_by(L), % assert the type of the table
        empty_group_by(L,t,LOUT).



/*
  There are THREE different predicates where the first argument is the EMPTY list: []

  If the actual 'group by' statement is missing from the original SQL code, then this is
  'bare aggregation', and we need to use the FIRST TWO predicates (and omit the third).

  If there is a 'group by' clause in the original SQL code, then we are grouping and so we
  leave the FIRST TWO predicates COMMENTED OUT and we use the THIRD predicate only.
*/
% pared everything down to an empty list, but there is also an EMPTY MAP (the so-far map)

empty_group_by([],t,MAP) :-

        % there should be 1 line of 'agg_field' statement for each column in the table
        agg_empty_max_atom(COL_1_AGG),
        agg_empty_do_nothing(COL_2_AGG),

        put_assoc(empty_table,
                  t,
                  (COL_1_AGG,COL_2_AGG),
                  MAP).


% after recursing down to an empty list-of-tuples, we can only consider the MAP to be the output if MAP is not empty:
empty_group_by([],MAP,MAP) :-
        \+empty_assoc(MAP).



% take the list-of-tuples, our 'so-far' map, and produce a done-map.
% in this case, the current row does group together with some already mapped key.
empty_group_by(
  [(COL_1,COL_2)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2),

        get_assoc((nogroupby), % map key needs to be instantiated by here.
                  MAP,
                  (COL_1_SOFAR,COL_2_SOFAR)),

        % there should be 1 line of 'agg_field' statement for each column in the table
        agg_field_max_atom(COL_1_SOFAR,COL_1,COL_1_AGG),
        agg_field_do_nothing(COL_2_SOFAR,COL_2,COL_2_AGG),

        put_assoc((nogroupby),
                  MAP,
                  (COL_1_AGG,COL_2_AGG),
                  MAP2),

        empty_group_by(LT,MAP2,MAP_OUT).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
% in this case, the group-key for the current row is new (never-seen so far), so we put starting values in the map for this key.
empty_group_by(
  [(COL_1,COL_2)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2),

        \+get_assoc((nogroupby),MAP,_), % map key needs to be instantiated by here.

        % there should be 1 line of 'agg_base' statement for each column in the table
        agg_base_max_atom(COL_1,COL_1_AGG),
        agg_base_do_nothing(COL_2,COL_2_AGG),

        put_assoc((nogroupby),
                  MAP,
                  (COL_1_AGG,COL_2_AGG),
                  MAP2),
        empty_group_by(LT,MAP2,MAP_OUT).







test_empty_group_by(X,K) :-

        empty_group_by(X,Y), assoc_to_values(Y,K).
