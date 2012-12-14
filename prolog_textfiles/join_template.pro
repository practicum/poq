



% ----------------------------------------------------------

/*
  Names of the placeholders in the template:

  # STCJRT sequence of tuple components for join result tuple type#
  # STCT1T sequence of tuple components for table-one tuple#
  # STCT2T table-two #
*/

/*
There are 7 different clauses to express join_on_expression.

There should be no duplication in outcomes due to careful management
of when each of the 7 clauses is allowed to be applied.

Each one of the 7 handles a NON-OVERLAPPING subset of cases based on
the SIZE of the first two list variables.

The cases (by size of the two lists) are:

[]    []
1+    []
[]    1+
1     >1
1+    1     (1+ means 'one or more')
2+    2+  ... and the first list size is greater to or EQUAL to the second
2+    2+  ... and the first list size is LESS THAN the second
*/


table_one_table(L) :-
        predicate_for_table_1(L).

table_two_table(L) :-
        predicate_for_table_2(L).

table_one_tuple(# STCT1T #) :-
        particulartable_tuple(# STCT1T #). % to be filled in on a case-by-case basis

table_two_table(# STCT2T #) :-
        particulartable2_tuple(# STCT2T #). % to be filled in on a case-by-case basis


% NOTE: let this always be named simply 'meets_join' throughout the system
meets_join(   # STCJRT #   ) :-
        ONE_ID_one = ONE_ID_two. % this can change and be arbitrarily complex


% case 1 of 7: left-hand list and right-hand list are [], []
join_on_expression( [], [], [] ).

% case 2 of 7: left-hand list and right-hand list are sizes: 1+, []
join_on_expression(
  [(  # STCT1T # ) |L2T],
  [],
  [] ) :-

        table_one_table([( # STCT1T # )   |L2T]), % type assertion

        within_table_size_limit_limit([( # STCT1T # )   |L2T]).


% case 3 of 7: left-hand list and right-hand list are sizes: [], 1+
join_on_expression(
  [],
  [( # STCT2T # )   |L2T],
  [] ) :-

        table_two_table([( # STCT2T # )   |L2T]), % type assertion

        within_table_size_limit_limit([( # STCT2T # )   |L2T]).


% case 4 of 7 - A: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, MEETS JOIN conditions
join_on_expression(
  [( # STCT1T # )   |[]],

  [( # STCT2T # )   |[MID2|L2T]],

  [( # STCJRT # ) |R] ) :-


        table_one_tuple( # STCT1T # ),  % type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]), % type assertion

        within_joined_size_limit([( # STCJRT # ) |R]),

        meets_join( # STCJRT # ),

        join_on_expression([( # STCT1T # )   |[]] , [MID2|L2T], R ).


% case 4 of 7 - B: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, FAILS TO MEET JOIN conditions
join_on_expression(
  [( # STCT1T # )   |[]],

  [( # STCT2T # )   |[MID2|L2T]],

  R ) :-

        table_one_tuple( # STCT1T # ),% type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]),% type assertion

        within_joined_size_limit(R),

        \+meets_join( # STCJRT # ),

        join_on_expression([( # STCT1T # )   |[]] , [MID2|L2T], R ).


% case 5 of 7 - A: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, MEETS JOIN conditions
join_on_expression(
  [( # STCT1T # )   |L2T],

  [( # STCT2T # )   |[]],

  [( # STCJRT # ) |R] ) :-


        table_one_table([( # STCT1T # )   |L2T]),% type assertion

        table_two_tuple( # STCT2T # ),% type assertion

        within_joined_size_limit([( # STCJRT # ) |R]),

        meets_join( # STCJRT # ),

        join_on_expression( L2T, [( # STCT2T # )   |[]] ,   R ).


% case 5 of 7 - B: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, FAILS TO MEET JOIN conditions
join_on_expression(
  [( # STCT1T # )   |L2T],

  [( # STCT2T # )   |[]],

  R ) :-

        table_one_table([( # STCT1T # )   |L2T]),% type assertion

        table_two_tuple( # STCT2T # ),% type assertion

        within_joined_size_limit(R),

        \+meets_join( # STCJRT # ),

        join_on_expression( L2T, [( # STCT2T # )   |[]] ,   R ).


% case 6 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is greater to or EQUAL to the second
% adding one more right-hand-list item to an 'already crossing'
join_on_expression(
  [( # STCT1T # )   |[MID1|L1T]],

  [( # STCT2T # )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( # STCT1T # )   |[MID1|L1T]]),% type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]),% type assertion

        length([( # STCT1T # )   |[MID1|L1T]],  X),
        length([( # STCT2T # )   |[MID2|L2T]],  Y),

        X>=Y,
        join_on_expression([( # STCT1T # )   |[MID1|L1T]],    [MID2|L2T],      POUT),
        join_on_expression([( # STCT1T # )   |[MID1|L1T]], [( # STCT2T # )   |[]],  MOUT),
        merge(POUT,MOUT,FINAL).



% case 7 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is LESS THAN the second
% adding one more left-hand-list item to an 'already crossing'
join_on_expression(
  [( # STCT1T # )   |[MID1|L1T]],

  [( # STCT2T # )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( # STCT1T # )   |[MID1|L1T]]),% type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]),% type assertion

        length([( # STCT1T # )   |[MID1|L1T]],  X),
        length([( # STCT2T # )   |[MID2|L2T]],  Y),

        X<Y,
        join_on_expression([MID1|L1T],  [( # STCT2T # )   |[MID2|L2T]],   POUT),
        join_on_expression([( # STCT1T # )   |[]],   [( # STCT2T # )   |[MID2|L2T]],    MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------



