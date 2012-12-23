

% ----------------------------------------------------------

/*
  to fill in the 'group by' template:

  determine the number of columns in the table to be grouped. use that number to
  replace or expand the instances of:
       (COL_1,COL_2,COL_3,COL_4)  # an input tuple
       (COL_1_SOFAR,COL_2_SOFAR,COL_3_SOFAR,COL_4_SOFAR)  # an aggregated tuple from the working map
       (COL_1_AGG,COL_2_AGG,COL_3_AGG,COL_4_AGG) # a just-computed tuple about to be placed into the map

  implement 'required_table_type_for_group_by' with the correct table type predicate.

  also implement 'required_tuple_type_for_group_by' with the correct tuple predicate.

  also implement restrict_list_tail_size as needed. sometimes we may need to assert the same size limit
  as for a 'base table', and other times we might be working with a join result (derived table), and
  therefore may need to assert a join-sized table.

  when using the group_by predicates, be sure to call use_module for the 'dbms_builtins' module,
  because that is where the aggregate helper functions are defined.
*/


/*
  still todo: must account for NULL values in all aggregates.

  note: the final map can be examined with: assoc_to_list, assoc_to_values

  possibly also with failure-driven backtracking:
    gen_assoc(?Key, +Assoc, ?Value)
      Enumerate matching elements of Assoc in ascending order of their keys via backtracking.
*/
group_by(L,LOUT) :-

        required_table_type_for_group_by(L), % assert the type of the table
        group_by(L,t,LOUT).



/*
  There are THREE different predicates where the first argument is the EMPTY list: []

  If the actual 'group by' statement is missing from the original SQL code, then this is
  'bare aggregation', and we need to use the FIRST TWO predicates (and omit the third).

  In that case (case of missing group by), then we also replace the GROUP KEY with 'nogroup'.

  If there is a 'group by' clause in the original SQL code, then we are grouping and so we
  leave the FIRST TWO predicates COMMENTED OUT and we use the THIRD predicate only.
*/
% pared everything down to an empty list, but there is also an EMPTY MAP (the so-far map)
group_by([],t,MAP) :-

        % there should be 1 line of 'agg_field' statement for each column in the table
        agg_empty_max_atom(COL_1_AGG),
        agg_empty_max_atom(COL_2_AGG),
        agg_empty_do_nothing(COL_3_AGG),
        agg_empty_do_nothing(COL_4_AGG),

        put_assoc(empty_table,
                  t,
                  (COL_1_AGG,COL_2_AGG,COL_3_AGG,COL_4_AGG),
                  MAP).


% after recursing down to an empty list-of-tuples, we can only consider the MAP to be the output if MAP is not empty:
group_by([],MAP,MAP) :-
        \+empty_assoc(MAP).


% nothing in the list for further processing. so your 'map so-far' is your finished map.
group_by([],MAP,MAP) :-

        write( '   -----------------------   ' ), nl.


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
% in this case, the current row does group together with some already mapped key.
group_by(
  [(COL_1,COL_2,COL_3,COL_4)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2,COL_3,COL_4),

        get_assoc((COL_4), % map key needs to be instantiated by here.
                  MAP,
                  (COL_1_SOFAR,COL_2_SOFAR,COL_3_SOFAR,COL_4_SOFAR)),

        % there should be 1 line of 'agg_field' statement for each column in the table
        agg_field_max_atom(COL_1_SOFAR,COL_1,COL_1_AGG),
        agg_field_max_atom(COL_2_SOFAR,COL_2,COL_2_AGG),
        agg_field_do_nothing(COL_3_SOFAR,COL_3,COL_3_AGG),
        agg_field_do_nothing(COL_4_SOFAR,COL_4,COL_4_AGG),

        put_assoc((COL_4),
                  MAP,
                  (COL_1_AGG,COL_2_AGG,COL_3_AGG,COL_4_AGG),
                  MAP2),

        group_by(LT,MAP2,MAP_OUT).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
% in this case, the group-key for the current row is new (never-seen so far), so we put starting values in the map for this key.
group_by(
  [(COL_1,COL_2,COL_3,COL_4)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2,COL_3,COL_4),

        \+get_assoc((COL_4),MAP,_), % map key needs to be instantiated by here.

        % there should be 1 line of 'agg_base' statement for each column in the table
        agg_base_max_atom(COL_1,COL_1_AGG),
        agg_base_max_atom(COL_2,COL_2_AGG),
        agg_base_do_nothing(COL_3,COL_3_AGG),
        agg_base_do_nothing(COL_4,COL_4_AGG),

        put_assoc((COL_4),
                  MAP,
                  (COL_1_AGG,COL_2_AGG,COL_3_AGG,COL_4_AGG),
                  MAP2),
        group_by(LT,MAP2,MAP_OUT).




test_it(X,K) :-

        group_by(X,Y), assoc_to_values(Y,K).