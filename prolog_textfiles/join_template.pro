



% ----------------------------------------------------------

/*
There are 7 different clauses to express sc_join_cd_on_EXPR.

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


% TODO - NOTE: let this always be named simply 'meets_join' throughout the system
meets_join(   # STCJRT sequence of tuple components for join result tuple type#   ) :-
        ONE_ID_one = ONE_ID_two. % this can change and be arbitrarily complex


% case 1 of 7: left-hand list and right-hand list are [], []
sc_join_cd_on_EXPR( [], [], [] ).

% case 2 of 7: left-hand list and right-hand list are sizes: 1+, []
sc_join_cd_on_EXPR(
  [(  # STCT1T sequence of tuple components for table-one tuple# ) |L2T],
  [],
  [] ) :-

        table_one_table([( # STCT1T # )   |L2T]), % type assertion

        within_table_size_limit([( # STCT1T # )   |L2T]).


% case 3 of 7: left-hand list and right-hand list are sizes: [], 1+
sc_join_cd_on_EXPR(
  [],
  [( # STCT2T # )   |L2T],
  [] ) :-

        table_two_table([( # STCT2T # )   |L2T]), % type assertion

        within_table_size_limit([( # STCT2T # )   |L2T]).


% case 4 of 7 - A: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [( # STCT1T # )   |[]],

  [( # STCT2T # )   |L2T],

  [( # STCJRT # ) |R] ) :-


        table_one_tuple( # STCT1T # ),  % type assertion

        table_two_table([( # STCT2T # )   |L2T]), % type assertion

        length([( # STCT2T # )   |L2T],X),
        X>1,
        within_table_size_limit(L2T),

        meets_join( # STCJRT # ),

        sc_join_cd_on_EXPR([( # STCT1T # )   |[]] , L2T, R ).


% case 4 of 7 - B: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [( # STCT1T # )   |[]],

  [( # STCT2T # )   |L2T],

  R ) :-

        table_one_tuple( # STCT1T # ),% type assertion

        table_two_table([( # STCT2T # )   |L2T]),% type assertion

        length([( # STCT2T # )   |L2T],X),

        X>1,
        within_table_size_limit(L2T),

        \+meets_join( # STCJRT # ),

        sc_join_cd_on_EXPR([( # STCT1T # )   |[]] , L2T, R ).


% case 5 of 7 - A: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [( # STCT1T # )   |L2T],

  [( # STCT2T # )   |[]],

  [( # STCJRT # ) |R] ) :-


        table_one_table([( # STCT1T # )   |L2T]),% type assertion

        table_two_tuple( # STCT2T # ),% type assertion

        within_table_size_limit(L2T),

        meets_join( # STCJRT # ),

        sc_join_cd_on_EXPR( L2T, [( # STCT2T # )   |[]] ,   R ).


% case 5 of 7 - B: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [( # STCT1T # )   |L2T],

  [( # STCT2T # )   |[]],

  R ) :-

        table_one_table([( # STCT1T # )   |L2T]),% type assertion

        table_two_tuple( # STCT2T # ),% type assertion

        within_table_size_limit(L2T),

        \+meets_join( # STCJRT # ),

        sc_join_cd_on_EXPR( L2T, [( # STCT2T # )   |[]] ,   R ).


% case 6 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is greater to or EQUAL to the second
% adding one more right-hand-list item to an 'already crossing'
sc_join_cd_on_EXPR(
  [( # STCT1T # )   |L1T],

  [( # STCT2T # )   |L2T],

  FINAL ) :-

        table_one_table([( # STCT1T # )   |L1T]),% type assertion

        table_two_table([( # STCT2T # )   |L2T]),% type assertion

        length([( # STCT1T # )   |L1T],  X),
        X>1,
        length([( # STCT2T # )   |L2T],  Y),
        Y>1,
        X>=Y,
        sc_join_cd_on_EXPR([( # STCT1T # )   |L1T],    L2T,      POUT),
        sc_join_cd_on_EXPR([( # STCT1T # )   |L1T], [( # STCT2T # )   |[]],  MOUT),
        merge(POUT,MOUT,FINAL).



% case 7 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is LESS THAN the second
% adding one more left-hand-list item to an 'already crossing'
sc_join_cd_on_EXPR(
  [( # STCT1T # )   |L1T],

  [( # STCT2T # )   |L2T],

  FINAL ) :-

        table_one_table([( # STCT1T # )   |L1T]),% type assertion

        table_two_table([( # STCT2T # )   |L2T]),% type assertion

        length([( # STCT1T # )   |L1T],  X),
        X>1,
        length([( # STCT2T # )   |L2T],  Y),

        Y>1,
        X<Y,
        sc_join_cd_on_EXPR(L1T,  [( # STCT2T # )   |L2T],   POUT),
        sc_join_cd_on_EXPR([( # STCT1T # )   |[]],   [( # STCT2T # )   |L2T],    MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------



