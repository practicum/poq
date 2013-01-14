

:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

%:- use_module(modules/dbms/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

/*
axiomatized_query(Person,ExtraInfo,Q_RESULT),
member( (ID,_,_), Q_RESULT ),
member( (ID,mr), ExtraInfo ).

length(Person,3),
axiomatized_query(Person,ExtraInfo,Q_RESULT),
member( (ID,_,_), Q_RESULT ),
member( (ID,mr), ExtraInfo ).


Person = [2, 0, 1],
ExtraInfo = [ (0, mrs), (1, mr)],
Q_RESULT = [ (0, 0, mrs), (2, null, null), (1, null, null)],
ID = 1





*/



% ----------------------------------------------------------

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


person_table_with_constraints(
  [(PID)   |LT],
  MAP,
  CURR_MAX,
  [(PID)   |REST]) :-

        within_table_size_limit([(PID)   |LT]),
        person_tuple(PID),

        \+get_assoc((PID),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((PID),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        person_table_with_constraints(LT,MAP2,LT_MAX,REST),
        person_tuple_in_order(PID,LT_MAX,CURR_MAX).


% ----------------------------------------------------------


extra_info_table(L) :-
        % t is the empty mapping, from library assoc
        extra_info_table_with_constraints(L,t,_,L).


extra_info_table_with_constraints([],_ASSOC,0,[]).


extra_info_table_with_constraints(
  [(PID,TITLE)   |LT],
  MAP,
  CURR_MAX,
  [(PID,TITLE)   |REST]) :-

        within_table_size_limit([(PID,TITLE)   |LT]),
        extra_info_tuple(PID,TITLE),

        \+get_assoc((PID),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((PID),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        extra_info_table_with_constraints(LT,MAP2,LT_MAX,REST),
        extra_info_tuple_in_order(PID,TITLE,LT_MAX,CURR_MAX).


% ----------------------------------------------------------



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
        person_table(L).

table_two_table(L) :-
        extra_info_table(L).

table_one_tuple(PID) :-
        person_tuple(PID). % to be filled in on a case-by-case basis

table_two_tuple(PID2,TITLE) :-
        extra_info_tuple(PID2,TITLE). % to be filled in on a case-by-case basis



left_join_on_expression(T1,T2,JT) :-
        internal_join_axioms(T1,T2,JA,T1,T1R),
        pad_right_side(T1R,T1RP),
        merge(JA,T1RP,JT).


pad_right_side([],[]).

pad_right_side([(PID)|OK1],[(PID , null,null )|OK2]) :-
        table_one_tuple( PID ),
        pad_right_side(OK1,OK2).


% look towards end of this file for the 'meets_join' predicate


% case 1 of 7: left-hand list and right-hand list are [], []
internal_join_axioms( [], [], [], T1R, T1R ).

% case 2 of 7: left-hand list and right-hand list are sizes: 1+, []
internal_join_axioms(
  [(  PID ) |L2T],
  [],
  [],
  T1R,
  T1R) :-

        table_one_table([( PID )   |L2T]), % type assertion

        within_table_size_limit([( PID )   |L2T]).


% case 3 of 7: left-hand list and right-hand list are sizes: [], 1+
internal_join_axioms(
  [],
  [( PID2,TITLE )   |L2T],
  [],
  T1R,
  T1R) :-

        table_two_table([( PID2,TITLE )   |L2T]), % type assertion

        within_table_size_limit([( PID2,TITLE )   |L2T]).


% case 4 of 7 - A: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, MEETS JOIN conditions
internal_join_axioms(
  [( PID )   |[]],

  [( PID2,TITLE )   |[MID2|L2T]],

  [( PID,PID2,TITLE ) |R],

  T1RA,

  T1RC) :-


        table_one_tuple( PID ),  % type assertion

        table_two_table([( PID2,TITLE )   |[MID2|L2T]]), % type assertion

        within_joined_size_limit([( PID,PID2,TITLE ) |R]),

        meets_join( PID,PID2,TITLE ),

        delete(T1RA,( PID ),T1RB),

        internal_join_axioms([( PID )   |[]] , [MID2|L2T], R, T1RB, T1RC ).


% case 4 of 7 - B: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( PID )   |[]],

  [( PID2,TITLE )   |[MID2|L2T]],

  R,

  T1RA,

  T1RB) :-

        table_one_tuple( PID ),% type assertion

        table_two_table([( PID2,TITLE )   |[MID2|L2T]]),% type assertion

        within_joined_size_limit(R),

        \+meets_join( PID,PID2,TITLE ),

        internal_join_axioms([( PID )   |[]] , [MID2|L2T], R, T1RA, T1RB ).


% case 5 of 7 - A: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, MEETS JOIN conditions
internal_join_axioms(
  [( PID )   |L2T],

  [( PID2,TITLE )   |[]],

  [( PID,PID2,TITLE ) |R],

  T1RA,

  T1RC) :-


        table_one_table([( PID )   |L2T]),% type assertion

        table_two_tuple( PID2,TITLE ),% type assertion

        within_joined_size_limit([( PID,PID2,TITLE ) |R]),

        meets_join( PID,PID2,TITLE ),

        delete(T1RA,( PID ),T1RB),

        internal_join_axioms( L2T, [( PID2,TITLE )   |[]] ,   R, T1RB, T1RC ).


% case 5 of 7 - B: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, FAILS TO MEET JOIN conditions
internal_join_axioms(
  [( PID )   |L2T],

  [( PID2,TITLE )   |[]],

  R,

  T1RA,

  T1RB) :-

        table_one_table([( PID )   |L2T]),% type assertion

        table_two_tuple( PID2,TITLE ),% type assertion

        within_joined_size_limit(R),

        \+meets_join( PID,PID2,TITLE ),

        internal_join_axioms( L2T, [( PID2,TITLE )   |[]] ,   R, T1RA, T1RB ).


% case 6 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is greater to or EQUAL to the second
% adding one more right-hand-list item to an 'already crossing'
internal_join_axioms(
  [( PID )   |[MID1|L1T]],

  [( PID2,TITLE )   |[MID2|L2T]],

  FINAL,

  T1RA,

  T1RZ) :-

        table_one_table([( PID )   |[MID1|L1T]]),% type assertion

        table_two_table([( PID2,TITLE )   |[MID2|L2T]]),% type assertion

        length([( PID )   |[MID1|L1T]],  X),
        length([( PID2,TITLE )   |[MID2|L2T]],  Y),

        X>=Y,
        internal_join_axioms([( PID )   |[MID1|L1T]],    [MID2|L2T],      POUT, T1RA, T1RB),
        internal_join_axioms([( PID )   |[MID1|L1T]], [( PID2,TITLE )   |[]],  MOUT, T1RB, T1RZ),
        merge(POUT,MOUT,FINAL).



% case 7 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is LESS THAN the second
% adding one more left-hand-list item to an 'already crossing'
internal_join_axioms(
  [( PID )   |[MID1|L1T]],

  [( PID2,TITLE )   |[MID2|L2T]],

  FINAL,

  T1RA,

  T1RZ) :-

        table_one_table([( PID )   |[MID1|L1T]]),% type assertion

        table_two_table([( PID2,TITLE )   |[MID2|L2T]]),% type assertion

        length([( PID )   |[MID1|L1T]],  X),
        length([( PID2,TITLE )   |[MID2|L2T]],  Y),

        X<Y,
        internal_join_axioms([MID1|L1T],  [( PID2,TITLE )   |[MID2|L2T]],   POUT, T1RA, T1RB),
        internal_join_axioms([( PID )   |[]],   [( PID2,TITLE )   |[MID2|L2T]],    MOUT, T1RB, T1RZ),
        merge(POUT,MOUT,FINAL).



% ----------------------------------------------------------

apply_where_clause([],[]).

apply_where_clause([(PID1,PID2,TITLE)|L1T],[(PID1,PID2,TITLE)|L2T]) :-
        natural_type(PID1),
        natural_type(PID2),
        title_string_type(TITLE),
        expression_1(TITLE),
        apply_where_clause(L1T,L2T).

apply_where_clause([(PID1,PID2,TITLE)|L1T],L2T) :-
        natural_type(PID1),
        natural_type(PID2),
        title_string_type(TITLE),
        \+expression_1(TITLE),
        apply_where_clause(L1T,L2T).



meets_join(   PID,PID2,_TITLE   ) :-
        PID = PID2. % this can change and be arbitrarily complex

% this is QC, which is well-behaved
axiomatized_query(Person,ExtraInfo,Q_RESULT) :-
        left_join_on_expression(Person,ExtraInfo,JT),
        apply_where_clause(JT,Q_RESULT).


/*
meets_join(   PID,PID2,TITLE   ) :-
        PID = PID2,
        expression_1(TITLE).

% this is QD, which is flawed
axiomatized_query(Person,ExtraInfo,Q_RESULT) :-
        left_join_on_expression(Person,ExtraInfo,Q_RESULT).

*/

