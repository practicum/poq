



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

shopping_cart_table(L) :-
        % t is the empty mapping, from library assoc
        shopping_cart_table_with_constraints(L,t,_,L).

shopping_cart_table_with_constraints([],_ASSOC,0,[]).

% Note: 'LT' stands for 'list tail'
shopping_cart_table_with_constraints(
  [ (CRT_sc,CART_DATE)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (CRT_sc,CART_DATE)  |LT2]
  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (CRT_sc,CART_DATE)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        shopping_cart_tuple(CRT_sc,CART_DATE),

        %negation on next line means key is not yet in map
        \+get_assoc((CRT_sc),MAP,_EXISTSVAL),
        put_assoc((CRT_sc),MAP,inmap,MAP2),
        shopping_cart_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        shopping_cart_tuple_in_order(CRT_sc,CART_DATE,LT_MAX,MAX).

% ----------------------------------------------------------

cart_detail_table(L) :-
        % t is the empty mapping, from library assoc
        cart_detail_table_with_constraints(L,t,_,L).

cart_detail_table_with_constraints([],_ASSOC,0,[]).

% Note: 'LT' stands for 'list tail'
cart_detail_table_with_constraints(
  [ (CART_cd,PRODUCT)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (CART_cd,PRODUCT)  |LT2]
  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (CART_cd,PRODUCT)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        cart_detail_tuple(CART_cd,PRODUCT),

        %negation on next line means key is not yet in map
        \+get_assoc((CART_cd,PRODUCT),MAP,_EXISTSVAL),
        put_assoc((CART_cd,PRODUCT),MAP,inmap,MAP2),
        cart_detail_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        cart_detail_tuple_in_order(CART_cd,PRODUCT,LT_MAX,MAX).

% ----------------------------------------------------------



/*
There are 9 different clauses to express join_on_expression.

For a complete discussion of why there are 9, please refer to
commentary inside the file 'template_join.pro'
*/


table_one_table(L) :-
        shopping_cart_table(L).

table_two_table(L) :-
        cart_detail_table(L).

table_one_tuple(CART_sc,CART_DATE) :-
        shopping_cart_tuple(CART_sc,CART_DATE).

table_two_tuple(CART_cd,PRODUCT) :-
        cart_detail_tuple(CART_cd,PRODUCT).


/*
  Note: to produce a Cartesian cross-product, simply let the body
  of 'meets_join' look like:

  meets_join(   CART_sc,CART_DATE,CART_cd,PRODUCT   ) :-
        true.
*/
meets_join(   CART_sc,_CART_DATE,CART_cd,_PRODUCT   ) :-
        CART_sc = CART_cd.


% case 1 of 7: list sizes are: [], []
join_on_expression( [], [], [] ).

% case 2 of 7: list sizes are: 1+, []
join_on_expression(
  [(  CART_sc,CART_DATE ) |L2T],
  [],
  [] ) :-

        table_one_table([( CART_sc,CART_DATE )   |L2T]),

        within_table_size_limit([( CART_sc,CART_DATE )  |L2T]).


% case 3 of 7: list sizes are: [], 1+
join_on_expression(
  [],
  [( CART_cd,PRODUCT )   |L2T],
  [] ) :-

        table_two_table([( CART_cd,PRODUCT )   |L2T]),

        within_table_size_limit([( CART_cd,PRODUCT )   |L2T]).


% case 4 of 7 - A: list sizes are: 1, >1
% 4A: concatenated output-tuple MEETS JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |[]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  [( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R] ) :-


        table_one_tuple( CART_sc,CART_DATE ),

        table_two_table([( CART_cd,PRODUCT )   |[MID2|L2T]]),

        within_joined_size_limit(
          [( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R]),

        meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression(
          [( CART_sc,CART_DATE ) |[]] ,[MID2|L2T], R ).


% case 4 of 7 - B: list sizes are: 1, >1
% 4B: concatenated output-tuple FAILS TO MEET JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |[]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  R ) :-

        table_one_tuple( CART_sc,CART_DATE ),

        table_two_table([( CART_cd,PRODUCT )   |[MID2|L2T]]),

        within_joined_size_limit(R),

        \+meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression(
          [( CART_sc,CART_DATE ) |[]] ,[MID2|L2T], R ).


% case 5 of 7 - A: list sizes are: 1+, 1 (1+ means 'one or more')
% 5A: concatenated output-tuple MEETS JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |L2T],

  [( CART_cd,PRODUCT )   |[]],

  [( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R] ) :-


        table_one_table([( CART_sc,CART_DATE )   |L2T]),

        table_two_tuple( CART_cd,PRODUCT ),

        within_joined_size_limit(
          [( CART_sc,CART_DATE,CART_cd,PRODUCT ) |R]),

        meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression( L2T, [(CART_cd,PRODUCT) |[]] , R ).


% case 5 of 7 - B: list sizes are: 1+, 1 (1+ means 'one or more')
% 5B: concatenated output-tuple FAILS TO MEET JOIN conditions
join_on_expression(
  [( CART_sc,CART_DATE )   |L2T],

  [( CART_cd,PRODUCT )   |[]],

  R ) :-

        table_one_table([( CART_sc,CART_DATE )   |L2T]),

        table_two_tuple( CART_cd,PRODUCT ),

        within_joined_size_limit(R),

        \+meets_join( CART_sc,CART_DATE,CART_cd,PRODUCT ),

        join_on_expression( L2T, [(CART_cd,PRODUCT) |[]] , R ).


/*
 case 6 of 7: list sizes are: 2+ 2+ ... and the first list size
 is greater to or EQUAL to the second

 this predicate states that in order to join a left-hand table
 with a right-hand table, first compute the join of the left-hand
 table with the TAIL of the right-hand table list, then compute
 the join of the left-hand table with the HEAD of the right-hand
 table list, then merge those two intermediate results.
*/
join_on_expression(
  [( CART_sc,CART_DATE )   |[MID1|L1T]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  FINAL ) :-

    table_one_table([( CART_sc,CART_DATE ) |[MID1|L1T]]),

    table_two_table([( CART_cd,PRODUCT ) |[MID2|L2T]]),

    length([( CART_sc,CART_DATE ) |[MID1|L1T]],  X),
    length([( CART_cd,PRODUCT ) |[MID2|L2T]],  Y),

    X>=Y,
    join_on_expression(
       [( CART_sc,CART_DATE ) |[MID1|L1T]],
       [MID2|L2T],
       POUT),
    join_on_expression(
       [( CART_sc,CART_DATE ) |[MID1|L1T]],
       [( CART_cd,PRODUCT ) |[]],
       MOUT),

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
join_on_expression(
  [( CART_sc,CART_DATE )   |[MID1|L1T]],

  [( CART_cd,PRODUCT )   |[MID2|L2T]],

  FINAL ) :-

    table_one_table([( CART_sc,CART_DATE ) |[MID1|L1T]]),

    table_two_table([( CART_cd,PRODUCT ) |[MID2|L2T]]),

    length([( CART_sc,CART_DATE ) |[MID1|L1T]],  X),
    length([( CART_cd,PRODUCT ) |[MID2|L2T]],  Y),

    X<Y,
    join_on_expression(
       [MID1|L1T],
       [( CART_cd,PRODUCT ) |[MID2|L2T]],
       POUT),
    join_on_expression(
       [( CART_sc,CART_DATE ) |[]],
       [( CART_cd,PRODUCT )   |[MID2|L2T]],
       MOUT),

    merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------


% Note: required_table_type_for_group_by not used here, because nothing in this example
% needed to use the group_by predicate taking only two terms.

required_tuple_type_for_group_by( CART,CART_DATE,CART,PRODUCT ) :-

        shopping_cart_tuple(CART,CART_DATE),
        cart_detail_tuple(CART,PRODUCT).


restrict_list_tail_size(LT) :-
        within_joined_size_limit(LT).


% nothing in the list for further processing. so your 'map
% so-far' is your finished map.
group_by([],MAP,MAP) :-

        write( '   -----------------------   ' ), nl.


/*
 this predicate describes how to take the list-of-tuples, our
 'so-far' map, and produce a done-map.

  in this case, the current row >does< group together with some
  already mapped key.
*/
group_by(
  [(COL_1,COL_2,COL_3,COL_4)   |LT],
  MAP,
  MAP_OUT ) :-

     restrict_list_tail_size(LT),

     required_tuple_type_for_group_by(COL_1,COL_2,COL_3,COL_4),

     get_assoc((COL_4),
               MAP,
               (COL_1_SOFAR,COL_2_SOFAR,COL_3_SOFAR,COL_4_SOFAR)),

     % there should be 1 line of 'agg_field*' statement for each
     % column in the table
     agg_field_max_atom(COL_1_SOFAR,COL_1,COL_1_AGG),
     agg_field_max_atom(COL_2_SOFAR,COL_2,COL_2_AGG),
     agg_field_max_atom(COL_3_SOFAR,COL_3,COL_3_AGG),
     agg_field_do_nothing(COL_4_SOFAR,COL_4,COL_4_AGG),

     put_assoc((COL_4),
               MAP,
               (COL_1_AGG,COL_2_AGG,COL_3_AGG,COL_4_AGG),
               MAP2),

     group_by(LT,MAP2,MAP_OUT).

/*
 this predicate describes how to take the list-of-tuples, our
 'so-far' map, and produce a done-map.

  in this case, the group-key for the current row is new
  (never-seen so far), so we put starting values in the map for
  this key.
*/
group_by(
  [(COL_1,COL_2,COL_3,COL_4)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2,COL_3,COL_4),

        \+get_assoc((COL_4),MAP,_),

        % there should be 1 line of 'agg_base*' statement for
        % each column in the table
        agg_base_max_atom(COL_1,COL_1_AGG),
        agg_base_max_atom(COL_2,COL_2_AGG),
        agg_base_max_atom(COL_3,COL_3_AGG),
        agg_base_do_nothing(COL_4,COL_4_AGG),

        put_assoc((COL_4),
                  MAP,
                  (COL_1_AGG,COL_2_AGG,COL_3_AGG,COL_4_AGG),
                  MAP2),
        group_by(LT,MAP2,MAP_OUT).





axiomatized_query(ShoppingCart,CartDetail,Q_RESULT) :-

        join_on_expression(ShoppingCart,CartDetail,J),
        group_by(J,t,LOUT),
        assoc_to_values(LOUT,Q_RESULT).

