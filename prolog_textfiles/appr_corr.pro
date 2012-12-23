

:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

%:- use_module(modules/dbms/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

/*
as of Nov 13, 2012, with this setting:

manageable_list_tail(L) :- size_0_to_2(L).


  i was able to do the following:

  appr_corr_join_derived_table( [(fccy463, 0, fccy463, aspirin), (srce544, 0,srce544, aspirin)] ).


?- test_it(  [(fccy463, 0, fccy463, aspirin), (srce544, 0,srce544, aspirin)]  ,K).
K = [g(d_prod_cart(0, aspirin, srce544), 2)] .


  ?- test_it(  [(fccy463, X, fccy463, aspirin), (srce544, Y,srce544, aspirin)]  , [g(d_prod_cart(X, aspirin, srce544), 2)]  ),
  X@>Y.


X = 1,
Y = 0 ;
X = 1,
Y = 0 ;
X = 2,
Y = 0 ;
X = 2,
Y = 0 ;
X = 2,
Y = 1 .



*/





shopping_cart_tuple(
  CART,
  CART_DATE) :-

        guid_type(CART), not_null(CART),
        natural_type(CART_DATE), not_null(CART_DATE).

shopping_cart_tuple_in_order(
  CART,
  CART_DATE,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_guid(CART,0,V0),
        map_natural(CART_DATE,1,V1),
        RANK_OF_THIS_TUPLE is V0 + V1,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

cart_detail_tuple(
  CART,
  PRODUCT) :-

        guid_type(CART), not_null(CART),
        product_string_type(PRODUCT), not_null(PRODUCT).

cart_detail_tuple_in_order(
  CART,
  PRODUCT,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_guid(CART,0,V0),
        map_product(PRODUCT,1,V1),
        RANK_OF_THIS_TUPLE is V0 + V1,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.



% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?

/*
shopping_cart_table_with_constraints(
  [(CART_sc,CART_DATE)   |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        shopping_cart_tuple(CART_sc,CART_DATE),

        get_assoc(CART_sc,MAP,_EXISTSVAL), % map key (CART_sc) needs to be instantiated by here.

        % in this case, 'get_assoc' was specified to be true, so this key value is already in the list.
        shopping_cart_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.
*/

shopping_cart_table(L) :-
        % t is the empty mapping, from library assoc
        shopping_cart_table_with_constraints(L,t,_,L).


shopping_cart_table_with_constraints([],_ASSOC,0,[]).


shopping_cart_table_with_constraints(
  [(CART_sc,CART_DATE)   |LT],
  MAP,
  CURR_MAX,
  [(CART_sc,CART_DATE)   |REST]) :-

        within_table_size_limit([(CART_sc,CART_DATE)   |LT]),
        shopping_cart_tuple(CART_sc,CART_DATE),

        \+get_assoc((CART_sc),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((CART_sc),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        shopping_cart_table_with_constraints(LT,MAP2,LT_MAX,REST),
        shopping_cart_tuple_in_order(CART_sc,CART_DATE,LT_MAX,CURR_MAX).



% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?

cart_detail_table(L) :-
        % t is the empty mapping, from library assoc
        cart_detail_table_with_constraints(L,t,_,L).


cart_detail_table_with_constraints([],_ASSOC,0,[]).


cart_detail_table_with_constraints(
  [(CART_cd,PRODUCT)   |LT],
  MAP,
  CURR_MAX,
  [(CART_cd,PRODUCT)   |REST]) :-

        within_table_size_limit([(CART_cd,PRODUCT)   |LT]),
        cart_detail_tuple(CART_cd,PRODUCT),

        \+get_assoc((CART_cd,PRODUCT),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((CART_cd,PRODUCT),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        cart_detail_table_with_constraints(LT,MAP2,LT_MAX,REST),
        cart_detail_tuple_in_order(CART_cd,PRODUCT,LT_MAX,CURR_MAX).

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
        shopping_cart_table(L).

table_two_table(L) :-
        cart_detail_table(L).

table_one_tuple(CART_sc,CART_DATE) :-
        shopping_cart_tuple(CART_sc,CART_DATE).

table_two_tuple(CART_cd,PRODUCT) :-
        cart_detail_tuple(CART_cd,PRODUCT).


% NOTE: let this always be named simply 'meets_join' throughout the system
meets_join(   CART_sc,_CART_DATE,CART_cd,_PRODUCT   ) :-
        CART_sc = CART_cd.


% case 1 of 7: left-hand list and right-hand list are [], []
join_on_expression( [], [], [] ).

% case 2 of 7: left-hand list and right-hand list are sizes: 1+, []
join_on_expression(
  [(  CART_sc,CART_DATE ) |L2T],
  [],
  [] ) :-

        table_one_table([( CART_sc,CART_DATE )   |L2T]), % type assertion

        within_table_size_limit([( CART_sc,CART_DATE )   |L2T]).


% case 3 of 7: left-hand list and right-hand list are sizes: [], 1+
join_on_expression(
  [],
  [( CART_cd,PRODUCT )   |L2T],
  [] ) :-

        table_two_table([( CART_cd,PRODUCT )   |L2T]), % type assertion

        within_table_size_limit([( CART_cd,PRODUCT )   |L2T]).


% case 4 of 7 - A: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, MEETS JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |[]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  [( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R] ) :-


        table_one_tuple( CART_sc,CART_DATE ),  % type assertion

        table_two_table([( CART_cd,PRODUCT )   |[MID2|L2T]]), % type assertion

        within_joined_size_limit([( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R]),

        meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression([( CART_sc,CART_DATE )   |[]] , [MID2|L2T], R ).


% case 4 of 7 - B: left-hand list and right-hand list are sizes: 1, >1
% single item in left-hand list but longer right-hand list, FAILS TO MEET JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |[]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  R ) :-

        table_one_tuple( CART_sc,CART_DATE ),% type assertion

        table_two_table([( CART_cd,PRODUCT )   |[MID2|L2T]]),% type assertion

        within_joined_size_limit(R),

        \+meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression([( CART_sc,CART_DATE )   |[]] , [MID2|L2T], R ).


% case 5 of 7 - A: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, MEETS JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |L2T],

  [( CART_cd,PRODUCT )   |[]],

  [( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R] ) :-


        table_one_table([( CART_sc,CART_DATE )   |L2T]),% type assertion

        table_two_tuple( CART_cd,PRODUCT ),% type assertion

        within_joined_size_limit([( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R]),

        meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression( L2T, [( CART_cd,PRODUCT )   |[]] ,   R ).


% case 5 of 7 - B: left-hand list and right-hand list are sizes: 1+, 1 (1+ means 'one or more')
% longer left-hand list but only a single item in right-hand list, FAILS TO MEET JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |L2T],

  [( CART_cd,PRODUCT )   |[]],

  R ) :-

        table_one_table([( CART_sc,CART_DATE )   |L2T]),% type assertion

        table_two_tuple( CART_cd,PRODUCT ),% type assertion

        within_joined_size_limit(R),

        \+meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression( L2T, [( CART_cd,PRODUCT )   |[]] ,   R ).


% case 6 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is greater to or EQUAL to the second
% adding one more right-hand-list item to an 'already crossing'
join_on_expression(
  [( CART_sc,CART_DATE )   |[MID1|L1T]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( CART_sc,CART_DATE )   |[MID1|L1T]]),% type assertion

        table_two_table([( CART_cd,PRODUCT )   |[MID2|L2T]]),% type assertion

        length([( CART_sc,CART_DATE )   |[MID1|L1T]],  X),
        length([( CART_cd,PRODUCT )   |[MID2|L2T]],  Y),

        X>=Y,
        join_on_expression([( CART_sc,CART_DATE )   |[MID1|L1T]],    [MID2|L2T],      POUT),
        join_on_expression([( CART_sc,CART_DATE )   |[MID1|L1T]], [( CART_cd,PRODUCT )   |[]],  MOUT),
        merge(POUT,MOUT,FINAL).



% case 7 of 7: left-hand list and right-hand list are sizes:
%  2+    2+  ... and the first list size is LESS THAN the second
% adding one more left-hand-list item to an 'already crossing'
join_on_expression(
  [( CART_sc,CART_DATE )   |[MID1|L1T]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  FINAL ) :-

        table_one_table([( CART_sc,CART_DATE )   |[MID1|L1T]]),% type assertion

        table_two_table([( CART_cd,PRODUCT )   |[MID2|L2T]]),% type assertion

        length([( CART_sc,CART_DATE )   |[MID1|L1T]],  X),
        length([( CART_cd,PRODUCT )   |[MID2|L2T]],  Y),

        X<Y,
        join_on_expression([MID1|L1T],  [( CART_cd,PRODUCT )   |[MID2|L2T]],   POUT),
        join_on_expression([( CART_sc,CART_DATE )   |[]],   [( CART_cd,PRODUCT )   |[MID2|L2T]],    MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------





% ----------------------------------------------------------



restrict_list_tail_size(LT) :-
        within_joined_size_limit(LT).

required_table_type_for_group_by(J) :-
        join_on_expression(_SC,_CD,J).

required_tuple_type_for_group_by( CART,CART_DATE,CART,PRODUCT ) :-

        shopping_cart_tuple(CART,CART_DATE),
        cart_detail_tuple(CART,PRODUCT).


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

