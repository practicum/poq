/*
  https://github.com/practicum/poq

  Filename: template_left_join.pro

  Axiom schema for binary operation 'left_join_on_expression.'

  The code below is a template. Filling in the parameters of the
  template will produce usable Prolog axioms.
*/

/*
  The meanings of the acronyms used in this file are:

      STCJR : sequence of tuple-components in join result
      STCT1 : sequence of tuple-components in table 1
      STCT2 : sequence of tuple-components in table 2

  Four parameters need filling in each time this template is
  employed:

  # T2NULLSPAD # :
        fill in with comma separated 'chain-of-nulls' that has
        the same arity as the T2 table operand. For example: if
        table 2 has two columns, then T2NULLSPAD should be:
        null,null

  # STCJR # :
        fill in with comma separated list of domain variables,
        one variable per tuple-component according to the tuple
        structure of the output of this join.  names should
        be valid Prolog variable names.
        for example: CART_sc,CART_DATE,CART_cd,PRODUCT

  # STCT1 # :
        fill in with comma separated list of domain variables,
        one variable per tuple-component according to the tuple
        structure of the left-hand operand table.  names should
        be valid Prolog variable names.

  # STCT2 # :
        fill in with comma separated list of domain variables,
        one variable per tuple-component according to the tuple
        structure of the right-hand operand table.  names should
        be valid Prolog variable names.

  In addition to filling in the template paramers shown above, it
  is also necessary to implement the following four predicates:

      table_one_table
      table_two_table
      table_one_tuple
      table_two_tuple
      meets_join

  The first four predicates should simply assert the relevant
  table type or tuple type. (for examples, see the example file
  02_appr_corr.pro)

  The fifth predicate--'meets_join'--accepts terms representing a
  potential tuple that may or may not belong to the output of
  this operation. 'meets_join' provides the logical test that
  determines whether the tuple is in the output or not.  Since
  this is a LEFT JOIN, a tuple that fails the test is not
  completely excluded from the output.  Rather, the portion of
  that tuple that derived from the left-hand table operand will
  be null-extended, and the null-extended tuple is output.
*/

% ----------------------------------------------------------

/*
There are 9 different clauses to express internal_join_axioms.

These represent 7 different ways that ordered pairs of table
operands can be categorized based on the respective size of each
operand.

After accounting for the 7 different sizing scenarios, scenarios
4 and 5 are both subdivided further into scenarios 4A, 4B, 5A,
and 5B, which is how we end up with 9 clauses in all.  The
subdivision of 4 and 5 is based on whether a concatenation of the
'head tuple' (the first) from each base table is a concatenated
tuple that MEETS or that DOES NOT MEET the join condition.

There should be no duplication in outcomes due to careful
management of when each of the 9 clauses is allowed to be
applied.

Each one of the 7 scenarios represents a NON-OVERLAPPING subset
of cases based on the SIZE of the first two list variables.

The 7 scenarios (by size of the two lists) are:

[]  []
1+  []    (1+ means 'one or more')
[]  1+
1   >1
1+  1
2+  2+  ... and 1st list size is greater to or EQUAL to the 2nd
2+  2+  ... and 1st list size is LESS THAN the 2nd
*/

table_one_table(L) :-
        predicate_for_table_1(L).

table_two_table(L) :-
        predicate_for_table_2(L).


% 'particulartable_tuple' is to be REPLACED by the relevant tuple
% typing predicate
table_one_tuple(# STCT1 #) :-
        particulartable_tuple(# STCT1 #).

% 'particulartable2_tuple' is to be REPLACED by the relevant
% tuple typing predicate
table_two_tuple(# STCT2 #) :-
        particulartable2_tuple(# STCT2 #).


left_join_on_expression(T1,T2,JT) :-
        % the final term in internal_join_axioms (namely, T1R),
        % represents all rows from table T1 that did not
        % successfully join to anything in T2.
        internal_join_axioms(T1,T2,JA,T1,T1R),
        % rows in T1R (T1's so-called 'remainder') need to be
        % null-extended, which is done by pad_right_side
        pad_right_side(T1R,T1RP),
        merge(JA,T1RP,JT).


pad_right_side([],[]).

pad_right_side(
  [(# STCT1 #)|OK1],
  [(# STCT1 # , # T2NULLSPAD # )|OK2]) :-
        table_one_tuple( # STCT1 # ),
        pad_right_side(OK1,OK2).


meets_join(   # STCJR #   ) :-
        % the body of this clause can be arbitrarily complex
        ONE_ID_one = ONE_ID_two.


% case 1 of 7: list sizes are: [], []
internal_join_axioms( [], [], [], T1R, T1R ).

% case 2 of 7: list sizes are: 1+, []
internal_join_axioms(
  [(  # STCT1 # ) |L2T],
  [],
  [],
  T1R,
  T1R) :-

        table_one_table([( # STCT1 # )   |L2T]),

        within_table_size_limit([( # STCT1 # )   |L2T]).


% case 3 of 7: list sizes are: [], 1+
internal_join_axioms(
  [],
  [( # STCT2 # )   |L2T],
  [],
  T1R,
  T1R) :-

        table_two_table([( # STCT2 # )   |L2T]),

        within_table_size_limit([( # STCT2 # )   |L2T]).


% case 4 of 7 - A: list sizes are: 1, >1
% 4A: concatenated output-tuple MEETS JOIN conditions
internal_join_axioms(
  [( # STCT1 # )   |[]],

  [( # STCT2 # )   |[MID2|L2T]],

  [( # STCJR # ) |R],

  T1RA, % T1's remainder *before* consuming current T1 head

  T1RC  % T1's remainder *after* consuming current T1 head
  ) :-


        table_one_tuple( # STCT1 # ),

        table_two_table([( # STCT2 # )   |[MID2|L2T]]),

        within_joined_size_limit([( # STCJR # ) |R]),

        meets_join( # STCJR # ),

        % we now have one fewer 'remainder' item:
        delete(T1RA,( # STCT1 # ),T1RB),

        internal_join_axioms(
          [( # STCT1 # )   |[]] , [MID2|L2T], R, T1RB, T1RC ).


% case 4 of 7 - B: list sizes are: 1, >1
% 4B: concatenated output-tuple FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( # STCT1 # )   |[]],

  [( # STCT2 # )   |[MID2|L2T]],

  R,

  T1RA,

  T1RB) :-

        table_one_tuple( # STCT1 # ),

        table_two_table([( # STCT2 # )   |[MID2|L2T]]),

        within_joined_size_limit(R),

        \+meets_join( # STCJR # ),

        internal_join_axioms(
          [( # STCT1 # )   |[]] , [MID2|L2T], R, T1RA, T1RB ).


% case 5 of 7 - A: list sizes are: 1+, 1 (1+ means 'one or more')
% 5A: concatenated output-tuple MEETS JOIN conditions
internal_join_axioms(
  [( # STCT1 # )   |L2T],

  [( # STCT2 # )   |[]],

  [( # STCJR # ) |R],

  T1RA, % T1's remainder *before* consuming current T1 head

  T1RC  % T1's remainder *after* consuming current T1 head
  ) :-


        table_one_table([( # STCT1 # )   |L2T]),

        table_two_tuple( # STCT2 # ),

        within_joined_size_limit([( # STCJR # ) |R]),

        meets_join( # STCJR # ),

        % we now have one fewer 'remainder' item:
        delete(T1RA,( # STCT1 # ),T1RB),

        internal_join_axioms(
          L2T, [( # STCT2 # )   |[]] ,   R, T1RB, T1RC ).


% case 5 of 7 - B: list sizes are: 1+, 1 (1+ means 'one or more')
% 5B: concatenated output-tuple FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( # STCT1 # )   |L2T],

  [( # STCT2 # )   |[]],

  R,

  T1RA,

  T1RB) :-

        table_one_table([( # STCT1 # )   |L2T]),

        table_two_tuple( # STCT2 # ),

        within_joined_size_limit(R),

        \+meets_join( # STCJR # ),

        internal_join_axioms(
          L2T, [( # STCT2 # )   |[]] ,   R, T1RA, T1RB ).


/*
 case 6 of 7: list sizes are: 2+ 2+ ... and the first list size
 is greater to or EQUAL to the second

 this predicate states that in order to join a left-hand table
 with a right-hand table, first compute the join of the left-hand
 table with the TAIL of the right-hand table list, then compute
 the join of the left-hand table with the HEAD of the right-hand
 table list, then merge those two intermediate results.
*/
internal_join_axioms(
  [( # STCT1 # )   |[MID1|L1T]],

  [( # STCT2 # )   |[MID2|L2T]],

  FINAL,

  T1RA, % T1's remainder *before* the binary recursion

  T1RZ  % T1's remainder *after* the binary recursion
  ) :-

    table_one_table([( # STCT1 # ) |[MID1|L1T]]),

    table_two_table([( # STCT2 # ) |[MID2|L2T]]),

    length([( # STCT1 # ) |[MID1|L1T]],  X),
    length([( # STCT2 # ) |[MID2|L2T]],  Y),

    X>=Y,
    internal_join_axioms(
       [( # STCT1 # )   |[MID1|L1T]],
       [MID2|L2T],
       POUT,
       T1RA,
       T1RB),
    internal_join_axioms(
       [( # STCT1 # )   |[MID1|L1T]],
       [( # STCT2 # )   |[]],
       MOUT,
       T1RB,
       T1RZ),
    merge(POUT,MOUT,FINAL).


/*
  case 7 of 7: list sizes are: 2+ 2+ ... and the first list size
  is LESS THAN the second

 this predicate states that in order to join a right-hand table
 with a left-hand table, first compute the join of the TAIL of
 the left-hand table list with the (entire) right-hand list, then
 compute the join of the HEAD of the left-hand table list with
 the (entire) right-hand table, then merge those two intermediate
 results.
*/
internal_join_axioms(
  [( # STCT1 # )   |[MID1|L1T]],

  [( # STCT2 # )   |[MID2|L2T]],

  FINAL,

  T1RA, % T1's remainder *before* the binary recursion

  T1RZ  % T1's remainder *after* the binary recursion
  ) :-

    table_one_table([( # STCT1 # ) |[MID1|L1T]]),

    table_two_table([( # STCT2 # ) |[MID2|L2T]]),

    length([( # STCT1 # ) |[MID1|L1T]],  X),
    length([( # STCT2 # ) |[MID2|L2T]],  Y),

    X<Y,
    internal_join_axioms(
       [MID1|L1T],
       [( # STCT2 # )   |[MID2|L2T]],
       POUT,
       T1RA,
       T1RB),
    internal_join_axioms(
       [( # STCT1 # )   |[]],
       [( # STCT2 # )   |[MID2|L2T]],
       MOUT,
       T1RB,
       T1RZ),
    merge(POUT,MOUT,FINAL).