/*
  https://github.com/practicum/poq

  Axiomatized query from example 6 of Chapter 5.

  Using SWI-Prolog Version 6, example runs on Linux by invoking
  either of:
        ./prolog_driver.sh run_06a.pro # counterexample found
        ./prolog_driver.sh run_06b.pro # verification success
*/

/*
  example of asking 'can i ever have a supplier in the results
  that does NOT supply anything?'  (and the answer, presumed to
  be unexpected but helpful, is yes)
*/

/*
loop s
  loop p
     throw out this s when not find an sp row matching sp.s,sp.p
*/

supplier_tuple(S_ID_s) :-
        natural_type(S_ID_s), not_null(S_ID_s).

supplier_tuple_in_order(
  S_ID_s,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(S_ID_s,0,V0),
        RANK_OF_THIS_TUPLE is V0,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

spjoin_tuple(
  S_ID_sp,
  P_ID_sp) :-

        natural_type(S_ID_sp), not_null(S_ID_sp),
        natural_type(P_ID_sp), not_null(P_ID_sp).

spjoin_tuple_in_order(
  S_ID_sp,
  P_ID_sp,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(S_ID_sp,0,V0),
        map_natural(P_ID_sp,1,V1),
        RANK_OF_THIS_TUPLE is V0 + V1,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

part_tuple(P_ID_p) :-

        natural_type(P_ID_p), not_null(P_ID_p).

part_tuple_in_order(
  P_ID_p,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(P_ID_p,0,V0),
        RANK_OF_THIS_TUPLE is V0,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

% ----------------------------------------------------------

supplier_table(L) :-
        % t is the empty mapping, from library assoc
        supplier_table_with_constraints(L,t,_,L).

supplier_table_with_constraints([],_ASSOC,0,[]).

supplier_table_with_constraints(
  [ (S_ID_s)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (S_ID_s)  |LT2]  ) :-

        within_table_size_limit([ (S_ID_s)  |LT]),
        supplier_tuple(S_ID_s),

        %negation on next line means key is not yet in map
        \+get_assoc((S_ID_s),MAP,_EXISTSVAL),
        put_assoc((S_ID_s),MAP,inmap,MAP2),
        supplier_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        supplier_tuple_in_order(S_ID_s,LT_MAX,MAX).

% ----------------------------------------------------------

part_table(L) :-
        part_table_with_constraints(L,t,_,L).

part_table_with_constraints([],_ASSOC,0,[]).

part_table_with_constraints(
  [ (P_ID_p)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (P_ID_p)  |LT2]  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (P_ID_p)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        part_tuple(P_ID_p),

        %negation on next line means key is not yet in map
        \+get_assoc((P_ID_p),MAP,_EXISTSVAL),
        put_assoc((P_ID_p),MAP,inmap,MAP2),
        part_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        part_tuple_in_order(P_ID_p,LT_MAX,MAX).

% ----------------------------------------------------------

spjoin_table(L) :-
        spjoin_table_with_constraints(L,t,_,L).

spjoin_table_with_constraints([],_ASSOC,0,[]).

spjoin_table_with_constraints(
  [ (S_ID_sp,P_ID_sp)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (S_ID_sp,P_ID_sp)  |LT2]  ) :-

        within_table_size_limit([ (S_ID_sp,P_ID_sp)  |LT]),
        spjoin_tuple(S_ID_sp,P_ID_sp),

        %negation on next line means key is not yet in map
        \+get_assoc((S_ID_sp,P_ID_sp),MAP,_EXISTSVAL),
        put_assoc((S_ID_sp,P_ID_sp),MAP,inmap,MAP2),
        spjoin_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        spjoin_tuple_in_order(S_ID_sp,P_ID_sp,LT_MAX,MAX).

% ----------------------------------------------------------

table_one_table(L) :-
        supplier_table(L).

table_two_table(L) :-
        part_table(L).

table_one_tuple(S_ID_s) :-
        supplier_tuple(S_ID_s).

table_two_tuple(P_ID_p) :-
        part_tuple(P_ID_p).

meets_join(   _S_ID_s,_P_ID_p   ) :-
        true.


% case 1 of 7: list sizes are [], []
join_on_expression( [], [], [] ).

% case 2 of 7: list sizes are: 1+, []
join_on_expression(
  [(  S_ID_s ) |L2T],
  [],
  [] ) :-

        table_one_table([( S_ID_s )   |L2T]),

        within_table_size_limit([( S_ID_s )   |L2T]).


% case 3 of 7: list sizes are: [], 1+
join_on_expression(
  [],
  [( P_ID_p )   |L2T],
  [] ) :-

        table_two_table([( P_ID_p )   |L2T]),

        within_table_size_limit([( P_ID_p )   |L2T]).


% case 4 of 7 - A: list sizes are: 1, >1
% MEETS JOIN conditions
join_on_expression(
  [( S_ID_s )   |[]],

  [( P_ID_p )   |[MID2|L2T]],

  [( S_ID_s,P_ID_p ) |R] ) :-


        table_one_tuple( S_ID_s ),

        table_two_table([( P_ID_p )   |[MID2|L2T]]),

        within_joined_size_limit([( S_ID_s,P_ID_p ) |R]),

        meets_join( S_ID_s,P_ID_p ),

        join_on_expression([( S_ID_s )   |[]] , [MID2|L2T], R ).


% case 4 of 7 - B: list sizes are: 1, >1
% FAILS TO MEET JOIN conditions

% omitted because we are doing a full cross product. *everything*
% meets the join condition.


% case 5 of 7 - A: list sizes are: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand
% list, MEETS JOIN conditions
join_on_expression(
  [( S_ID_s )   |L2T],

  [( P_ID_p )   |[]],

  [( S_ID_s,P_ID_p ) |R] ) :-


        table_one_table([( S_ID_s )   |L2T]),

        table_two_tuple( P_ID_p ),

        within_joined_size_limit([( S_ID_s,P_ID_p ) |R]),

        meets_join( S_ID_s,P_ID_p ),

        join_on_expression( L2T, [( P_ID_p )   |[]] ,   R ).


% case 5 of 7 - B: list sizes are: 1+, 1 (1+ means 'one or more')
% FAILS TO MEET JOIN conditions

% omitted because we are doing a full cross product. *everything*
% meets the join condition.


% case 6 of 7: list sizes are: 2+ 2+ ... and the first list size
% is greater to or EQUAL to the second adding one more
% right-hand-list item to an 'already crossing'
join_on_expression(
  [( S_ID_s )   |[MID1|L1T]],

  [( P_ID_p )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( S_ID_s )   |[MID1|L1T]]),

        table_two_table([( P_ID_p )   |[MID2|L2T]]),

        length([( S_ID_s )   |[MID1|L1T]],  X),
        length([( P_ID_p )   |[MID2|L2T]],  Y),

        X>=Y,
        join_on_expression([( S_ID_s ) |[MID1|L1T]],
                           [MID2|L2T],
                           POUT),
        join_on_expression([( S_ID_s ) |[MID1|L1T]],
                           [( P_ID_p ) |[]],
                           MOUT),
        merge(POUT,MOUT,FINAL).



% case 7 of 7: list sizes are:
%  2+    2+  ... and the first list size is LESS THAN the second
% adding one more left-hand-list item to an 'already crossing'
join_on_expression(
  [( S_ID_s )   |[MID1|L1T]],

  [( P_ID_p )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( S_ID_s )   |[MID1|L1T]]),

        table_two_table([( P_ID_p )   |[MID2|L2T]]),

        length([( S_ID_s )   |[MID1|L1T]],  X),
        length([( P_ID_p )   |[MID2|L2T]],  Y),

        X<Y,
        join_on_expression([MID1|L1T],
                           [( P_ID_p ) |[MID2|L2T]],
                           POUT),
        join_on_expression([( S_ID_s ) |[]],
                           [( P_ID_p ) |[MID2|L2T]],
                           MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------


axiomatized_query(Supplier,Part,SPJoin,Q_RESULT) :-

        join_on_expression(Supplier,Part,XSP),
        spjoin_table(SPJoin),

        filter_supp(SPJoin,XSP,Supplier,Q_RESULT).


filter_supp(_SPJ_CONST,[],STAB_SUBSET,STAB_SUBSET) :-

        write( '   -----------------------   ' ), nl.


filter_supp(SPJ_CONST,
            [s_p(s(S_ID_s), (P_ID_p))|XSP_T],
            STAB_SUBSET,
            NEXT_SUBSET) :-

        \+member(sp(S_ID_s,P_ID_p) ,SPJ_CONST),
        delete(STAB_SUBSET,s(S_ID_s),NEW_SUBSET),
        filter_supp(SPJ_CONST,XSP_T,NEW_SUBSET,NEXT_SUBSET).

filter_supp(SPJ_CONST,
            [s_p(s(S_ID_s), (P_ID_p))|XSP_T],
            STAB_SUBSET,
            NEXT_SUBSET) :-

        member(sp(S_ID_s,P_ID_p) ,SPJ_CONST),
        filter_supp(SPJ_CONST,XSP_T,STAB_SUBSET,NEXT_SUBSET).