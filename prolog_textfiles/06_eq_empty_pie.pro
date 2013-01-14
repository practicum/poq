

:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

/*
example of asking 'can i ever have a supplier in the results that does NOT supply anything?'
  (and the answer, presumed to be unexpected but helpful, is yes)

run_query(S,P,XING,SP,Q),
  member((A),Q),
  \+member( (A,B), SP).

axiomatized_query(Supplier,Part,SPJoin,Q_RESULT),
  member( (SID), Q_RESULT ),
  \+member( (SID,_), SPJoin ).

Supplier = [0, 1],
Part = [],
SPJoin = [ (0, 0), (0, 1)],
Q_RESULT = [0, 1],
SID = 1

length(Part,L),
within_table_size_limit(Part),
L@>0,

  within_table_size_limit(Part),
  L@>0,
  axiomatized_query(Supplier,Part,SPJoin,Q_RESULT),
  member( (SID), Q_RESULT ),
  \+member( (SID,_), SPJoin ),
  length(Part,L),
  L@>0.

  axiomatized_query(Supplier,Part,SPJoin,Q_RESULT), member( (SID), Q_RESULT ), \+member( (SID,_), SPJoin ), length(Part,L), L@>0.

  */

/*
loop s
  loop p
     throw out this s if we do not find an sp row matching+ sp.s,sp.p
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

% putting the UNIQUE S_ID_s info here
supplier_table(L) :-
        % t is the empty mapping, from library assoc
        supplier_table_with_constraints(L,t,_,L).


supplier_table_with_constraints([],_ASSOC,0,[]).


supplier_table_with_constraints(
  [(S_ID_s)   |LT],
  MAP,
  CURR_MAX,
  [(S_ID_s)   |REST]) :-

        within_table_size_limit([(S_ID_s)   |LT]),
        supplier_tuple(S_ID_s),

        \+get_assoc((S_ID_s),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((S_ID_s),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        supplier_table_with_constraints(LT,MAP2,LT_MAX,REST),
        supplier_tuple_in_order(S_ID_s,LT_MAX,CURR_MAX).


% ----------------------------------------------------------

% putting the UNIQUE P_ID_p information here.
part_table(L) :-
        % t is the empty mapping, from library assoc
        part_table_with_constraints(L,t,_,L).


part_table_with_constraints([],_ASSOC,0,[]).


part_table_with_constraints(
  [(P_ID_p)   |LT],
  MAP,
  CURR_MAX,
  [(P_ID_p)   |REST]) :-

        within_table_size_limit([(P_ID_p)   |LT]),
        part_tuple(P_ID_p),

        \+get_assoc((P_ID_p),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((P_ID_p),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        part_table_with_constraints(LT,MAP2,LT_MAX,REST),
        part_tuple_in_order(P_ID_p,LT_MAX,CURR_MAX).


% ----------------------------------------------------------

% putting the UNIQUE (S_ID_sp,P_ID_sp) information here.
spjoin_table(L) :-
        % t is the empty mapping, from library assoc
        spjoin_table_with_constraints(L,t,_,L).


spjoin_table_with_constraints([],_ASSOC,0,[]).


spjoin_table_with_constraints(
  [(S_ID_sp,P_ID_sp)   |LT],
  MAP,
  CURR_MAX,
  [(S_ID_sp,P_ID_sp)   |REST]) :-

        within_table_size_limit([(S_ID_sp,P_ID_sp)   |LT]),
        spjoin_tuple(S_ID_sp,P_ID_sp),

        \+get_assoc((S_ID_sp,P_ID_sp),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((S_ID_sp,P_ID_sp),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        spjoin_table_with_constraints(LT,MAP2,LT_MAX,REST),
        spjoin_tuple_in_order(S_ID_sp,P_ID_sp,LT_MAX,CURR_MAX).


% ----------------------------------------------------------



% ----------------------------------------------------------

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
        supplier_table(L).

table_two_table(L) :-
        part_table(L).

table_one_tuple(S_ID_s) :-
        supplier_tuple(S_ID_s). % to be filled in on a case-by-case basis

table_two_tuple(P_ID_p) :-
        part_tuple(P_ID_p). % to be filled in on a case-by-case basis


% NOTE: let this always be named simply 'meets_join' throughout the system
meets_join(   _S_ID_s,_P_ID_p   ) :-
        true.



% case 1 of 7: left-hand list and right-hand list are [], []
join_on_expression( [], [], [] ).

% case 2 of 7: left-hand list and right-hand list are sizes: 1+, []
join_on_expression(
  [(  S_ID_s ) |L2T],
  [],
  [] ) :-

        table_one_table([( S_ID_s )   |L2T]), % type assertion

        within_table_size_limit([( S_ID_s )   |L2T]).


% case 3 of 7: left-hand list and right-hand list are sizes: [], 1+
join_on_expression(
  [],
  [( P_ID_p )   |L2T],
  [] ) :-

        table_two_table([( P_ID_p )   |L2T]), % type assertion

        within_table_size_limit([( P_ID_p )   |L2T]).


% case 4 of 7 - A: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, MEETS JOIN conditions
join_on_expression(
  [( S_ID_s )   |[]],

  [( P_ID_p )   |[MID2|L2T]],

  [( S_ID_s,P_ID_p ) |R] ) :-


        table_one_tuple( S_ID_s ),  % type assertion

        table_two_table([( P_ID_p )   |[MID2|L2T]]), % type assertion

        within_joined_size_limit([( S_ID_s,P_ID_p ) |R]),

        meets_join( S_ID_s,P_ID_p ),

        join_on_expression([( S_ID_s )   |[]] , [MID2|L2T], R ).


% case 4 of 7 - B: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, FAILS TO MEET JOIN conditions

% omitted because we are doing a full cross product. *everything* meets the join condition.


% case 5 of 7 - A: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, MEETS JOIN conditions
join_on_expression(
  [( S_ID_s )   |L2T],

  [( P_ID_p )   |[]],

  [( S_ID_s,P_ID_p ) |R] ) :-


        table_one_table([( S_ID_s )   |L2T]),% type assertion

        table_two_tuple( P_ID_p ),% type assertion

        within_joined_size_limit([( S_ID_s,P_ID_p ) |R]),

        meets_join( S_ID_s,P_ID_p ),

        join_on_expression( L2T, [( P_ID_p )   |[]] ,   R ).


% case 5 of 7 - B: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, FAILS TO MEET JOIN conditions

% omitted because we are doing a full cross product. *everything* meets the join condition.


% case 6 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is greater to or EQUAL to the second
% adding one more right-hand-list item to an 'already crossing'
join_on_expression(
  [( S_ID_s )   |[MID1|L1T]],

  [( P_ID_p )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( S_ID_s )   |[MID1|L1T]]),% type assertion

        table_two_table([( P_ID_p )   |[MID2|L2T]]),% type assertion

        length([( S_ID_s )   |[MID1|L1T]],  X),
        length([( P_ID_p )   |[MID2|L2T]],  Y),

        X>=Y,
        join_on_expression([( S_ID_s )   |[MID1|L1T]],    [MID2|L2T],      POUT),
        join_on_expression([( S_ID_s )   |[MID1|L1T]], [( P_ID_p )   |[]],  MOUT),
        merge(POUT,MOUT,FINAL).



% case 7 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is LESS THAN the second
% adding one more left-hand-list item to an 'already crossing'
join_on_expression(
  [( S_ID_s )   |[MID1|L1T]],

  [( P_ID_p )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( S_ID_s )   |[MID1|L1T]]),% type assertion

        table_two_table([( P_ID_p )   |[MID2|L2T]]),% type assertion

        length([( S_ID_s )   |[MID1|L1T]],  X),
        length([( P_ID_p )   |[MID2|L2T]],  Y),

        X<Y,
        join_on_expression([MID1|L1T],  [( P_ID_p )   |[MID2|L2T]],   POUT),
        join_on_expression([( S_ID_s )   |[]],   [( P_ID_p )   |[MID2|L2T]],    MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------

/*
  STAB - the actual Supplier table for this instance of the scenario.
  PTAB - the actual Part table for this instance of the scenario.
  XSP - a cartesian crossing of STAB and PTAB. (equivalent to nested loops: for s in Supp { for p in Part })

  SPJ - the actual SPJoin table. (IMPORTANT WARNING: not foreign-key constrained!)

  QR - result of query when performed on the above-mentioned objects (from example one of equal shares of an empty pie)


 */
axiomatized_query(Supplier,Part,SPJoin,Q_RESULT) :-

        join_on_expression(Supplier,Part,XSP),
        spjoin_table(SPJoin),

        filter_supp(SPJoin,XSP,Supplier,Q_RESULT).


filter_supp(_SPJ_CONST,[],STAB_SUBSET,STAB_SUBSET) :- % could check type of SPJ_CONST

        write( '   -----------------------   ' ), nl.
/*
  We need to recurse down through the whole MOD_XSP.

  SPJ_CONST will stay the same throughout.

  STAB_CONST will stay the same throughout.

  STAB_SUBSET needs to 'begin' (start out?) holding exactly the content of STAB_CONST

  - need the current head of MOD_XSP.
  - the current head has 's' and 'p'.
  - if s,p is NOT in SPJ_CONST, then *delete* 's' from STAB_SUBSET
  - otherwise, leave STAB_SUBSET unchanged and recurse on MOD_XSP tail
*/


filter_supp(SPJ_CONST,
            [s_p(s(S_ID_s), (P_ID_p))|XSP_T], %MOD_XSP,
            STAB_SUBSET,
            NEXT_SUBSET) :-

        \+member(sp(S_ID_s,P_ID_p) ,SPJ_CONST), % - if s,p is NOT in SPJ_CONST, then *delete* 's' from STAB_SUBSET
        delete(STAB_SUBSET,s(S_ID_s),NEW_SUBSET), % TODO -check behavior: if s is NOT in STAB. if s is in there twice?
        filter_supp(SPJ_CONST,XSP_T,NEW_SUBSET,NEXT_SUBSET).

filter_supp(SPJ_CONST,
            [s_p(s(S_ID_s), (P_ID_p))|XSP_T], %MOD_XSP,
            STAB_SUBSET,
            NEXT_SUBSET) :-

        member(sp(S_ID_s,P_ID_p) ,SPJ_CONST),
        filter_supp(SPJ_CONST,XSP_T,STAB_SUBSET,NEXT_SUBSET).%- otherwise, leave STAB_SUBSET unchanged and recurse on MOD_XSP tail




