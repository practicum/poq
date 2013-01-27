/*
  https://github.com/practicum/poq

  Axiomatized query from example 3 of Chapter 5.

  Using SWI-Prolog Version 6, example runs on Linux by invoking
  either of:
        ./prolog_driver.sh run_03a.pro # counterexample found
        ./prolog_driver.sh run_03b.pro # verification success
*/


% this expression is re-used. it can be the join criteria or the
% where-clause criteria.
expression_1(TITLE) :-

        TITLE \= mr.

% ----------------------------------------------------------

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

% Note: 'LT' stands for 'list tail'
person_table_with_constraints(
  [ (PID)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (PID)  |LT2]
  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (PID)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        person_tuple(PID),

        %negation on next line means key is not yet in map
        \+get_assoc((PID),MAP,_EXISTSVAL),
        put_assoc((PID),MAP,inmap,MAP2),
        person_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        person_tuple_in_order(PID,LT_MAX,MAX).


% ----------------------------------------------------------

extra_info_table(L) :-
        % t is the empty mapping, from library assoc
        extra_info_table_with_constraints(L,t,_,L).

extra_info_table_with_constraints([],_ASSOC,0,[]).

extra_info_table_with_constraints(
  [ (PID,TITLE)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (PID,TITLE)  |LT2]  ) :-

        within_table_size_limit([ (PID,TITLE)  |LT]),
        extra_info_tuple(PID,TITLE),

        %negation on next line means key is not yet in map
        \+get_assoc((PID),MAP,_EXISTSVAL),
        put_assoc((PID),MAP,inmap,MAP2),
        extra_info_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        extra_info_tuple_in_order(PID,TITLE,LT_MAX,MAX).

% ----------------------------------------------------------

/*
There are 9 different clauses to express internal_join_axioms.

For a complete discussion of why there are 9, please refer to
commentary inside the file 'template_left_join.pro'
*/

table_one_table(L) :-
        person_table(L).

table_two_table(L) :-
        extra_info_table(L).

table_one_tuple(PID) :-
        person_tuple(PID).

table_two_tuple(PID2,TITLE) :-
        extra_info_tuple(PID2,TITLE).

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
  [(PID)|OK1],
  [(PID , null,null )|OK2]) :-
        table_one_tuple( PID ),
        pad_right_side(OK1,OK2).


% look towards end of this file for the 'meets_join' predicate


% case 1 of 7: list sizes are: [], []
internal_join_axioms( [], [], [], T1R, T1R ).

% case 2 of 7: list sizes are: 1+, []
internal_join_axioms(
  [(  PID ) |L2T],
  [],
  [],
  T1R,
  T1R) :-

        table_one_table([( PID )   |L2T]),

        within_table_size_limit([( PID )   |L2T]).


% case 3 of 7: list sizes are: [], 1+
internal_join_axioms(
  [],
  [( PID2,TITLE )   |L2T],
  [],
  T1R,
  T1R) :-

        table_two_table([( PID2,TITLE )   |L2T]),

        within_table_size_limit([( PID2,TITLE )   |L2T]).


% case 4 of 7 - A: list sizes are: 1, >1
% 4A: concatenated output-tuple MEETS JOIN conditions
internal_join_axioms(
  [( PID )   |[]],

  [( PID2,TITLE )   |[MID2|L2T]],

  [( PID,PID2,TITLE ) |R],

  T1RA, % T1's remainder *before* consuming current T1 head

  T1RC  % T1's remainder *after* consuming current T1 head
  ) :-


        table_one_tuple( PID ),

        table_two_table([( PID2,TITLE )   |[MID2|L2T]]),

        within_joined_size_limit([( PID,PID2,TITLE ) |R]),

        meets_join( PID,PID2,TITLE ),

        % we now have one fewer 'remainder' item:
        delete(T1RA,( PID ),T1RB),

        internal_join_axioms(
          [( PID )   |[]] , [MID2|L2T], R, T1RB, T1RC ).


% case 4 of 7 - B: list sizes are: 1, >1
% 4B: concatenated output-tuple FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( PID )   |[]],

  [( PID2,TITLE )   |[MID2|L2T]],

  R,

  T1RA,

  T1RB) :-

        table_one_tuple( PID ),

        table_two_table([( PID2,TITLE )   |[MID2|L2T]]),

        within_joined_size_limit(R),

        \+meets_join( PID,PID2,TITLE ),

        internal_join_axioms(
          [( PID )   |[]] , [MID2|L2T], R, T1RA, T1RB ).


% case 5 of 7 - A: list sizes are: 1+, 1 (1+ means 'one or more')
% 5A: concatenated output-tuple MEETS JOIN conditions
internal_join_axioms(
  [( PID )   |L2T],

  [( PID2,TITLE )   |[]],

  [( PID,PID2,TITLE ) |R],

  T1RA, % T1's remainder *before* consuming current T1 head

  T1RC  % T1's remainder *after* consuming current T1 head
  ) :-


        table_one_table([( PID )   |L2T]),

        table_two_tuple( PID2,TITLE ),

        within_joined_size_limit([( PID,PID2,TITLE ) |R]),

        meets_join( PID,PID2,TITLE ),

        % we now have one fewer 'remainder' item:
        delete(T1RA,( PID ),T1RB),

        internal_join_axioms(
          L2T, [( PID2,TITLE )   |[]] ,   R, T1RB, T1RC ).


% case 5 of 7 - B: list sizes are: 1+, 1 (1+ means 'one or more')
% 5B: concatenated output-tuple FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( PID )   |L2T],

  [( PID2,TITLE )   |[]],

  R,

  T1RA,

  T1RB) :-

        table_one_table([( PID )   |L2T]),

        table_two_tuple( PID2,TITLE ),

        within_joined_size_limit(R),

        \+meets_join( PID,PID2,TITLE ),

        internal_join_axioms(
          L2T, [( PID2,TITLE )   |[]] ,   R, T1RA, T1RB ).


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
  [( PID )   |[MID1|L1T]],

  [( PID2,TITLE )   |[MID2|L2T]],

  FINAL,

  T1RA, % T1's remainder *before* the binary recursion

  T1RZ  % T1's remainder *after* the binary recursion
  ) :-

    table_one_table([( PID ) |[MID1|L1T]]),

    table_two_table([( PID2,TITLE ) |[MID2|L2T]]),

    length([( PID ) |[MID1|L1T]],  X),
    length([( PID2,TITLE ) |[MID2|L2T]],  Y),

    X>=Y,
    internal_join_axioms(
       [( PID )   |[MID1|L1T]],
       [MID2|L2T],
       POUT,
       T1RA,
       T1RB),
    internal_join_axioms(
       [( PID )   |[MID1|L1T]],
       [( PID2,TITLE )   |[]],
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
  [( PID )   |[MID1|L1T]],

  [( PID2,TITLE )   |[MID2|L2T]],

  FINAL,

  T1RA, % T1's remainder *before* the binary recursion

  T1RZ  % T1's remainder *after* the binary recursion
  ) :-

    table_one_table([( PID ) |[MID1|L1T]]),

    table_two_table([( PID2,TITLE ) |[MID2|L2T]]),

    length([( PID ) |[MID1|L1T]],  X),
    length([( PID2,TITLE ) |[MID2|L2T]],  Y),

    X<Y,
    internal_join_axioms(
       [MID1|L1T],
       [( PID2,TITLE )   |[MID2|L2T]],
       POUT,
       T1RA,
       T1RB),
    internal_join_axioms(
       [( PID )   |[]],
       [( PID2,TITLE )   |[MID2|L2T]],
       MOUT,
       T1RB,
       T1RZ),
    merge(POUT,MOUT,FINAL).

% ----------------------------------------------------------


meets_criteria_where_clause(_PID1,_PID2,TITLE) :-
        expression_1(TITLE).

filter_list_where_clause([],[]).

% Note: 'LT' stands for 'list tail'
filter_list_where_clause(
  [(PID1,PID2,TITLE)|LT],
  [(PID1,PID2,TITLE)|LT2]) :-

        natural_type(PID1),
        natural_type(PID2),
        title_string_type(TITLE),
        meets_criteria_where_clause(PID1,PID2,TITLE),
        filter_list_where_clause(LT,LT2).

filter_list_where_clause(
  [(PID1,PID2,TITLE)|LT],
  LT2) :-

        natural_type(PID1),
        natural_type(PID2),
        title_string_type(TITLE),
        \+meets_criteria_where_clause(PID1,PID2,TITLE),
        filter_list_where_clause(LT,LT2).

% ----------------------------------------------------------

/*
% this section of code will be used when you run:
%   ./prolog_driver.sh run_03a.pro

meets_join(   PID,PID2,TITLE   ) :-
        PID = PID2,
        expression_1(TITLE).

axiomatized_query(Person,ExtraInfo,Q_RESULT) :-
        left_join_on_expression(Person,ExtraInfo,Q_RESULT).

*/

% ----------------------------------------------------------

/*
% this section of code will be used when you run:
%   ./prolog_driver.sh run_03b.pro

meets_join(   PID,PID2,_TITLE   ) :-
        PID = PID2.

% this is QC, which is well-behaved
axiomatized_query(Person,ExtraInfo,Q_RESULT) :-
        left_join_on_expression(Person,ExtraInfo,JT),
        filter_list_where_clause(JT,Q_RESULT).
*/
