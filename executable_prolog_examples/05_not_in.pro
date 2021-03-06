/*
  https://github.com/practicum/poq

  Axiomatized query from example 5 of Chapter 5.

  Using SWI-Prolog Version 6, example runs on Linux by invoking
  either of:

  ./prolog_driver.sh run_05_notexists.pro # counterexample found
  ./prolog_driver.sh run_05_notin.pro   # verification success
*/

type_one_tuple(T01) :-
        natural_type(T01).

type_one_tuple_in_order(
  T01,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(T01,0,V0),
        RANK_OF_THIS_TUPLE is V0,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

type_two_tuple(T02) :-
        natural_type(T02), not_null(T02).

type_two_tuple_in_order(
  T02,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(T02,0,V0),
        RANK_OF_THIS_TUPLE is V0,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

% ----------------------------------------------------------

type_one_table(L) :-
        % t is the empty mapping, from library assoc
        type_one_table_with_constraints(L,t,_,L).

type_one_table_with_constraints([],_ASSOC,0,[]).

% Note: 'LT' stands for 'list tail'
type_one_table_with_constraints(
  [ (T01)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (T01)  |LT2]
  ) :-

        within_table_size_limit([ (T01)  |LT]),
        type_one_tuple(T01),

        %negation on next line means key is not yet in map
        \+get_assoc((T01),MAP,_EXISTSVAL),
        put_assoc((T01),MAP,inmap,MAP2),
        type_one_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        type_one_tuple_in_order(T01,LT_MAX,MAX).

% ----------------------------------------------------------

type_two_table(L) :-
        % t is the empty mapping, from library assoc
        type_two_table_with_constraints(L,t,_,L).

type_two_table_with_constraints([],_ASSOC,0,[]).

% Note: 'LT' stands for 'list tail'
type_two_table_with_constraints(
  [ (T02)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (T02)  |LT2]
  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (T02)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        type_two_tuple(T02),

        %negation on next line means key is not yet in map
        \+get_assoc((T02),MAP,_EXISTSVAL),
        put_assoc((T02),MAP,inmap,MAP2),
        type_two_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        type_two_tuple_in_order(T02,LT_MAX,MAX).

% ----------------------------------------------------------


/*
 T1 - the actual TypeOne table for this instance of the scenario.
 T2 - the actual TypeTwo table for this instance of the scenario.

 QR - result of query when performed on the above-mentioned
 objects (from 'null cannot not in' pitfall)
 */
run_query_not_exists(T1,T2,QR) :-

        type_one_table(T1),
        type_two_table(T2),

        filter_notexists(T1,T2,T1,QR).

filter_notexists([],_T2_CONST,T1_SUB,T1_SUB) :-
        write( '   -----------------------   ' ), nl.

/*
  We need to recurse down through the whole MOD_T1.

  T2_CONST will stay the same throughout.

  T1_SUBSET needs to 'begin' (start out?) holding exactly the
  content of T1.

- need the current head of MOD_T1.
- the current head has 't'.
- if t is NOT in T2_CONST, then *delete* 't' from T1_SUBSET
- otherwise, leave T1_SUBSET unchanged and recurse on MOD_T1 tail
*/


filter_notexists([(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        % - if t is not in T2_CONST, then keep it
        \+member((T01) ,T2_CONST),
        %- otherwise, leave STAB_SUBSET unchanged and recurse on
        %  MOD_XSP tail
        filter_notexists(T1_T,T2_CONST,T1_SUBSET,NEXT_SUBSET).

filter_notexists([(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        % - when we find t in T2_CONST, then delete it from our
        % result
        member((T01) ,T2_CONST),
        delete(T1_SUBSET,(T01),NEW_SUBSET),
        filter_notexists(T1_T,T2_CONST,NEW_SUBSET,NEXT_SUBSET).

/*
 T1 - the actual TypeOne table for this instance of the scenario.
 T2 - the actual TypeTwo table for this instance of the scenario.

 QR - result of query when performed on the above-mentioned
 objects (from 'null cannot not in' pitfall)
*/
run_query_not_in(T1,T2,QR) :-

        type_one_table(T1),
        type_two_table(T2),

        filter_notin(T1,T2,T1,QR).

filter_notin([],_T2_CONST,T1_SUB,T1_SUB) :- % could check types

        write( '   -----------------------   ' ), nl.

/*
  We need to recurse down through the whole MOD_T1.

  T2_CONST will stay the same throughout.

  T1_SUBSET needs to 'begin' (start out?) holding exactly the
  content of T1.

- need the current head of MOD_T1.
- the current head has 't'.
- if t is NOT in T2_CONST, then *delete* 't' from T1_SUBSET
- otherwise, leave T1_SUBSET unchanged and recurse on MOD_T1 tail
*/


filter_notin([(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        \+member((T01) ,T2_CONST),
        not_null(T01),
        filter_notin(T1_T,T2_CONST,T1_SUBSET,NEXT_SUBSET).

filter_notin([(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        T01 = null, % filter out 'null' from the results
        delete(T1_SUBSET,(T01),NEW_SUBSET),
        filter_notin(T1_T,T2_CONST,NEW_SUBSET,NEXT_SUBSET).


filter_notin([(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        member((T01) ,T2_CONST),
        delete(T1_SUBSET,(T01),NEW_SUBSET),
        filter_notin(T1_T,T2_CONST,NEW_SUBSET,NEXT_SUBSET).
