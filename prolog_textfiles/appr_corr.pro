

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

product_string_type(null).
product_string_type(aspirin).
product_string_type(ibuprofen).
%product_string_type(guaifenesin).



shopping_cart_tuple(
  CART,
  CART_DATE) :-

        guid_type(CART), not_null(CART),
        natural_type(CART_DATE), not_null(CART_DATE).

cart_detail_tuple(
  CART,
  PRODUCT) :-

        guid_type(CART), not_null(CART),
        product_string_type(PRODUCT), not_null(PRODUCT).




% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
shopping_cart_table(L) :-
        % t is the empty mapping, from library assoc
        shopping_cart_table_with_constraints(L,t,L).


shopping_cart_table_with_constraints([],_ASSOC,[]).


shopping_cart_table_with_constraints(
  [(CART_sc,           CART_DATE)   |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        shopping_cart_tuple(CART_sc,CART_DATE),

        get_assoc(CART_sc,MAP,_EXISTSVAL), % map key (CART_sc) needs to be instantiated by here.

        shopping_cart_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


shopping_cart_table_with_constraints(
  [(CART_sc,           CART_DATE)   |LT],
  MAP,
  [(CART_sc,
           CART_DATE)   |REST]) :-

        within_table_size_limit([(CART_sc,    CART_DATE)   |LT]),
        shopping_cart_tuple(CART_sc,CART_DATE),

        \+get_assoc(CART_sc,MAP,_EXISTSVAL),  % map key (CART_sc) needs to be instantiated by here.
        put_assoc(CART_sc,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        shopping_cart_table_with_constraints(LT,MAP2,REST).



% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
cart_detail_table(L) :-
        % t is the empty mapping, from library assoc
        cart_detail_table_with_constraints(L,t,L).


cart_detail_table_with_constraints([],_ASSOC,[]).


cart_detail_table_with_constraints(
  [(CART_cd,           PRODUCT)   |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        cart_detail_tuple(CART_cd,PRODUCT),

        get_assoc(ck(CART_cd,PRODUCT),MAP,_EXISTSVAL), % map key (CART_cd) needs to be instantiated by here.

        cart_detail_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


cart_detail_table_with_constraints(
  [(CART_cd,           PRODUCT)   |LT],
  MAP,
  [(CART_cd,
           PRODUCT)   |REST]) :-

        within_table_size_limit([(CART_cd,           PRODUCT)   |LT]),
        cart_detail_tuple(CART_cd,PRODUCT),

        \+get_assoc(ck(CART_cd,PRODUCT),MAP,_EXISTSVAL),  % map key (ck(CART_cd,PRODUCT)) needs to be instantiated by here.
        put_assoc(ck(CART_cd,PRODUCT),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        cart_detail_table_with_constraints(LT,MAP2,REST).


% ----------------------------------------------------------



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



meets_join(CART_sc, _CART_DATE, CART_cd,  _PRODUCT) :-

        CART_sc = CART_cd.



sc_join_cd_on_EXPR( [], [], [] ).


sc_join_cd_on_EXPR(
  [(CART_sc,CART_DATE) |L2T],
  [],
  [] ) :-

        shopping_cart_table([(CART_sc,CART_DATE)   |L2T]), % type assertion

        within_table_size_limit([(CART_sc,CART_DATE)   |L2T]).


sc_join_cd_on_EXPR(
  [],
  [(CART_cd,PRODUCT)   |L2T],
  [] ) :-

        cart_detail_table([(CART_cd,PRODUCT)   |L2T]), % type assertion

        within_table_size_limit([(CART_cd,PRODUCT)   |L2T]).



% single cart but longer list of c_details, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_sc,CART_DATE)   |[]],

  [(CART_cd,PRODUCT)   |L2T],

  [(CART_sc,CART_DATE,CART_cd,PRODUCT) |R] ) :-


        shopping_cart_tuple(CART_sc,CART_DATE),  % type assertion

        cart_detail_table([(CART_cd,PRODUCT)   |L2T]), % type assertion

        length([(CART_cd,PRODUCT)   |L2T],X),
        X>1,
        within_table_size_limit(L2T),

        meets_join(CART_sc,CART_DATE,CART_cd,PRODUCT),

        sc_join_cd_on_EXPR([(CART_sc,CART_DATE)   |[]] , L2T, R ).


% single cart but longer list of c_details, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_sc,CART_DATE)   |[]],

  [(CART_cd,PRODUCT)   |L2T],

  R ) :-

        shopping_cart_tuple(CART_sc,CART_DATE),% type assertion

        cart_detail_table([(CART_cd,PRODUCT)   |L2T]),% type assertion

        length([(CART_cd,PRODUCT)   |L2T],X),

        X>1,
        within_table_size_limit(L2T),

        \+meets_join(CART_sc,CART_DATE,CART_cd,PRODUCT),

        sc_join_cd_on_EXPR([(CART_sc,CART_DATE)   |[]] , L2T, R ).



% longer carts list but SINGLE detail item, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_sc,CART_DATE)   |L2T],

  [(CART_cd,PRODUCT)   |[]],

  [(CART_sc,CART_DATE,CART_cd,PRODUCT) |R] ) :-


        shopping_cart_table([(CART_sc,CART_DATE)   |L2T]),% type assertion

        cart_detail_tuple(CART_cd,PRODUCT),% type assertion

        within_table_size_limit(L2T),

        meets_join(CART_sc,CART_DATE,CART_cd,PRODUCT),

        sc_join_cd_on_EXPR( L2T, [(CART_cd,PRODUCT)   |[]] ,   R ).


% longer carts list but SINGLE details item, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_sc,CART_DATE)   |L2T],

  [(CART_cd,PRODUCT)   |[]],

  R ) :-

        shopping_cart_table([(CART_sc,CART_DATE)   |L2T]),% type assertion

        cart_detail_tuple(CART_cd,PRODUCT),% type assertion

        within_table_size_limit(L2T),

        \+meets_join(CART_sc,CART_DATE,CART_cd,PRODUCT),

        sc_join_cd_on_EXPR( L2T, [(CART_cd,PRODUCT)   |[]] ,   R ).



% adding one more details item to an 'already crossing'
sc_join_cd_on_EXPR(
  [(CART_sc,CART_DATE)   |L1T],

  [(CART_cd,PRODUCT)   |L2T],

  FINAL ) :-

        shopping_cart_table([(CART_sc,CART_DATE)   |L1T]),% type assertion

        cart_detail_table([(CART_cd,PRODUCT)   |L2T]),% type assertion

        length([(CART_sc,CART_DATE)   |L1T],  X),
        X>1,
        length([(CART_cd,PRODUCT)   |L2T],  Y),
        Y>1,
        X>=Y,
        sc_join_cd_on_EXPR([(CART_sc,CART_DATE)   |L1T],    L2T,      POUT),
        sc_join_cd_on_EXPR([(CART_sc,CART_DATE)   |L1T], [(CART_cd,PRODUCT)   |[]],  MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more cart to an 'already crossing'
sc_join_cd_on_EXPR(
  [(CART_sc,CART_DATE)   |L1T],

  [(CART_cd,PRODUCT)   |L2T],

  FINAL ) :-

        shopping_cart_table([(CART_sc,CART_DATE)   |L1T]),% type assertion

        cart_detail_table([(CART_cd,PRODUCT)   |L2T]),% type assertion

        length([(CART_sc,CART_DATE)   |L1T],  X),
        X>1,
        length([(CART_cd,PRODUCT)   |L2T],  Y),

        Y>1,
        X<Y,
        sc_join_cd_on_EXPR(L1T,  [(CART_cd,PRODUCT)   |L2T],   POUT),
        sc_join_cd_on_EXPR([(CART_sc,CART_DATE)   |[]],   [(CART_cd,PRODUCT)   |L2T],    MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------





% ----------------------------------------------------------

% we are grouping rows that look like: abc(fccy463, full_member_barcode, 0, tinyint_0)
% we are grouping based on BARCODE_TYPE. (the second field in the row)

% still todo: must account for NULL values in all aggregates.

% the aggregate function for field 1 is nothing-at-all.
% the aggregate function for BARCODE_TYPE (the GROUP_KEY) is count. handled specially since it is GROUP_KEY.
% the aggregate function for AMENITIES_ID is sum. (it makes no sense to sum a key, but this is a demo.)
% the aggregate function for IN_PLAY is min.


appr_corr_join_derived_table(J) :-
        sc_join_cd_on_EXPR(_SC,_CD,J).

appr_corr_join_derived_tuple( CART,CART_DATE,CART,PRODUCT ) :-

        shopping_cart_tuple(CART,CART_DATE),
        cart_detail_tuple(CART,PRODUCT).

/*
J = [sc_cd((fccy463, 0), cd(fccy463, rural)),
     sc_cd((fccy463, 0), cd(fccy463, noise))]
*/


/*
  note: the final map can be examined with: assoc_to_list, assoc_to_values

  possibly also with failure-driven backtracking:
    gen_assoc(?Key, +Assoc, ?Value)
      Enumerate matching elements of Assoc in ascending order of their keys via backtracking.
*/
appr_corr_group_by(L,LOUT) :-

        appr_corr_join_derived_table(L),
        appr_corr_group_by(L,t,LOUT).


% nothing in the list for further processing. so your 'map so-far' is your finished map.
appr_corr_group_by([],MAP,MAP) :-

        write( '   -----------------------   ' ), nl.

% (CART_sc,CART_DATE,CART_cd,PRODUCT_gk)

% take the list-of-tuples, our 'so-far' map, and produce a done-map.
appr_corr_group_by(
  [(CART_sc,CART_DATE,CART_cd,PRODUCT_gk)   |LT],
  MAP,
  MAP_OUT ) :-

        within_table_size_limit(LT),

        appr_corr_join_derived_tuple(CART_sc,CART_DATE,CART_cd,PRODUCT_gk),

        get_assoc(PRODUCT_gk, % map key (GROUP_KEY) needs to be instantiated by here.
                  MAP,
                  g(d_prod_cart(MAX_DATE,PRODUCT_gk,MAX_CART), COUNT) ),

        agg_field_max_atom(MAX_DATE,CART_DATE,STORE_CART_DATE),
        agg_field_max_atom(MAX_CART,CART_sc,STORE_CART),

        NEW_COUNT is COUNT + 1,

        put_assoc(PRODUCT_gk,
                  MAP,
                  g(d_prod_cart(STORE_CART_DATE,PRODUCT_gk,STORE_CART), NEW_COUNT),
                  MAP2),

        appr_corr_group_by(LT,MAP2,MAP_OUT).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
appr_corr_group_by(
  [(CART_sc,CART_DATE,CART_cd,PRODUCT_gk)   |LT],
  MAP,
  MAP_OUT ) :-

        within_table_size_limit(LT),

        appr_corr_join_derived_tuple(CART_sc,CART_DATE,CART_cd,PRODUCT_gk),

        \+get_assoc(PRODUCT_gk,MAP,_), % map key (PRODUCT_gk) needs to be instantiated by here.
        put_assoc(PRODUCT_gk,
                  MAP,
                  g(d_prod_cart(CART_DATE,PRODUCT_gk,CART_sc),  % aggregate: min
                    1), % note: 1 is the starting point for the count aggregate
                  MAP2),
        appr_corr_group_by(LT,MAP2,MAP_OUT).




test_it(X,K) :-

        appr_corr_group_by(X,Y), assoc_to_values(Y,K).
