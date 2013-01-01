




% ----------------------------------------------------------

/*
  Names of the placeholders in the template:

  # STCJRT sequence of tuple components for join result tuple type#
  # STCT1T sequence of tuple components for table-one tuple#
  # STCT2T table-two #
  # T2NULLSPAD # this will be a 'chain-of-nulls' that matches the arity of the T2 relation.
*/

/*
There are 7 different clauses to express internal_join_axioms.

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

table_two_tuple(# STCT2T #) :-
        particulartable2_tuple(# STCT2T #). % to be filled in on a case-by-case basis



left_join_on_expression(T1,T2,JT) :-
        internal_join_axioms(T1,T2,JA,T1,T1R),
        pad_right_side(T1R,T1RP),
        merge(JA,T1RP,JT).


pad_right_side([],[]).

pad_right_side([(# STCT1T #)|OK1],[(# STCT1T # , # T2NULLSPAD # )|OK2]) :-
        table_one_tuple( # STCT1T # ),
        pad_right_side(OK1,OK2).


% NOTE: let this always be named simply 'meets_join' throughout the system
meets_join(   # STCJRT #   ) :-
        ONE_ID_one = ONE_ID_two. % this can change and be arbitrarily complex


% case 1 of 7: left-hand list and right-hand list are [], []
internal_join_axioms( [], [], [], T1R, T1R ).

% case 2 of 7: left-hand list and right-hand list are sizes: 1+, []
internal_join_axioms(
  [(  # STCT1T # ) |L2T],
  [],
  [],
  T1R,
  T1R) :-

        table_one_table([( # STCT1T # )   |L2T]), % type assertion

        within_table_size_limit([( # STCT1T # )   |L2T]).


% case 3 of 7: left-hand list and right-hand list are sizes: [], 1+
internal_join_axioms(
  [],
  [( # STCT2T # )   |L2T],
  [],
  T1R,
  T1R) :-

        table_two_table([( # STCT2T # )   |L2T]), % type assertion

        within_table_size_limit([( # STCT2T # )   |L2T]).


% case 4 of 7 - A: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, MEETS JOIN conditions
internal_join_axioms(
  [( # STCT1T # )   |[]],

  [( # STCT2T # )   |[MID2|L2T]],

  [( # STCJRT # ) |R],

  T1RA,

  T1RC) :-


        table_one_tuple( # STCT1T # ),  % type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]), % type assertion

        within_joined_size_limit([( # STCJRT # ) |R]),

        meets_join( # STCJRT # ),

        delete(T1RA,( # STCT1T # ),T1RB),

        internal_join_axioms([( # STCT1T # )   |[]] , [MID2|L2T], R, T1RB, T1RC ).


% case 4 of 7 - B: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( # STCT1T # )   |[]],

  [( # STCT2T # )   |[MID2|L2T]],

  R,

  T1RA,

  T1RB) :-

        table_one_tuple( # STCT1T # ),% type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]),% type assertion

        within_joined_size_limit(R),

        \+meets_join( # STCJRT # ),

        internal_join_axioms([( # STCT1T # )   |[]] , [MID2|L2T], R, T1RA, T1RB ).


% case 5 of 7 - A: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, MEETS JOIN conditions
internal_join_axioms(
  [( # STCT1T # )   |L2T],

  [( # STCT2T # )   |[]],

  [( # STCJRT # ) |R],

  T1RA,

  T1RC) :-


        table_one_table([( # STCT1T # )   |L2T]),% type assertion

        table_two_tuple( # STCT2T # ),% type assertion

        within_joined_size_limit([( # STCJRT # ) |R]),

        meets_join( # STCJRT # ),

        delete(T1RA,( # STCT1T # ),T1RB),

        internal_join_axioms( L2T, [( # STCT2T # )   |[]] ,   R, T1RB, T1RC ).


% case 5 of 7 - B: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( # STCT1T # )   |L2T],

  [( # STCT2T # )   |[]],

  R,

  T1RA,

  T1RB) :-

        table_one_table([( # STCT1T # )   |L2T]),% type assertion

        table_two_tuple( # STCT2T # ),% type assertion

        within_joined_size_limit(R),

        \+meets_join( # STCJRT # ),

        internal_join_axioms( L2T, [( # STCT2T # )   |[]] ,   R, T1RA, T1RB ).


% case 6 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is greater to or EQUAL to the second
% adding one more right-hand-list item to an 'already crossing'
internal_join_axioms(
  [( # STCT1T # )   |[MID1|L1T]],

  [( # STCT2T # )   |[MID2|L2T]],

  FINAL,

  T1RA,

  T1RZ) :-

        table_one_table([( # STCT1T # )   |[MID1|L1T]]),% type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]),% type assertion

        length([( # STCT1T # )   |[MID1|L1T]],  X),
        length([( # STCT2T # )   |[MID2|L2T]],  Y),

        X>=Y,
        internal_join_axioms([( # STCT1T # )   |[MID1|L1T]],    [MID2|L2T],      POUT, T1RA, T1RB),
        internal_join_axioms([( # STCT1T # )   |[MID1|L1T]], [( # STCT2T # )   |[]],  MOUT, T1RB, T1RZ),
        merge(POUT,MOUT,FINAL).



% case 7 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is LESS THAN the second
% adding one more left-hand-list item to an 'already crossing'
internal_join_axioms(
  [( # STCT1T # )   |[MID1|L1T]],

  [( # STCT2T # )   |[MID2|L2T]],

  FINAL,

  T1RA,

  T1RZ) :-

        table_one_table([( # STCT1T # )   |[MID1|L1T]]),% type assertion

        table_two_table([( # STCT2T # )   |[MID2|L2T]]),% type assertion

        length([( # STCT1T # )   |[MID1|L1T]],  X),
        length([( # STCT2T # )   |[MID2|L2T]],  Y),

        X<Y,
        internal_join_axioms([MID1|L1T],  [( # STCT2T # )   |[MID2|L2T]],   POUT, T1RA, T1RB),
        internal_join_axioms([( # STCT1T # )   |[]],   [( # STCT2T # )   |[MID2|L2T]],    MOUT, T1RB, T1RZ),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------



