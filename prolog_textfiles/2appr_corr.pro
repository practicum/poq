

:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

%:- use_module(modules/dbms/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

/*
as of Nov 13, 2012, with this setting:

manageable_list_tail(L) :- size_0_to_2(L).


  i was able to do the following:

  ?- appr_corr_join_derived_table( [sc_cd(sc(fccy463, 0), cd(fccy463, rural)), sc_cd(sc(srce544, 0), cd(srce544, rural))] ).
true .

?- test_it(  [sc_cd(sc(fccy463, 0), cd(fccy463, rural)), sc_cd(sc(srce544, 0), cd(srce544, rural))]  ,K).K = [g(d_prod_cart(0, rural, srce544), 2)] .

?- test_it(  [sc_cd(sc(fccy463, X), cd(fccy463, rural)), sc_cd(sc(srce544, Y), cd(srce544, rural))]  ,  [g(d_prod_cart(X, rural, srce544), 2)]),X@>Y.
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



/*
    sc will stand for 'ShoppingCart'
    cd will stand for 'CartDetail'
*/


t_ShoppingCart(
  CART,
  CART_DATE) :-

        demoguid(CART), nonnull(CART),
        demonat(CART_DATE), nonnull(CART_DATE).

t_CartDetail(
  CART,
  PRODUCT) :-

        demoguid(CART), nonnull(CART),
        wordstr(PRODUCT), nonnull(PRODUCT).




% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
t_table_content_scart(L) :-
        % t is the empty mapping, from library assoc
        list_type_sc_removed_dup_cart(L,t,L).


list_type_sc_removed_dup_cart([],_ASSOC,[]).


list_type_sc_removed_dup_cart(
  [sc(CART_sc,
           CART_DATE)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_ShoppingCart(CART_sc,CART_DATE),

        get_assoc(CART_sc,MAP,_EXISTSVAL), % map key (CART_sc) needs to be instantiated by here.

        list_type_sc_removed_dup_cart(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_sc_removed_dup_cart(
  [sc(CART_sc,
           CART_DATE)   |LT],
  MAP,
  [sc(CART_sc,
           CART_DATE)   |REST]) :-

        manageable_list_tail(LT),
        t_ShoppingCart(CART_sc,CART_DATE),

        \+get_assoc(CART_sc,MAP,_EXISTSVAL),  % map key (CART_sc) needs to be instantiated by here.
        put_assoc(CART_sc,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_sc_removed_dup_cart(LT,MAP2,REST).



% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
t_table_content_cdetail(L) :-
        % t is the empty mapping, from library assoc
        list_type_cd_compound_key(L,t,L).


list_type_cd_compound_key([],_ASSOC,[]).


list_type_cd_compound_key(
  [cd(CART_cd,
           PRODUCT)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_CartDetail(CART_cd,PRODUCT),

        get_assoc(ck(CART_cd,PRODUCT),MAP,_EXISTSVAL), % map key (CART_cd) needs to be instantiated by here.

        list_type_cd_compound_key(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_cd_compound_key(
  [cd(CART_cd,
           PRODUCT)   |LT],
  MAP,
  [cd(CART_cd,
           PRODUCT)   |REST]) :-

        manageable_list_tail(LT),
        t_CartDetail(CART_cd,PRODUCT),

        \+get_assoc(ck(CART_cd,PRODUCT),MAP,_EXISTSVAL),  % map key (ck(CART_cd,PRODUCT)) needs to be instantiated by here.
        put_assoc(ck(CART_cd,PRODUCT),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_cd_compound_key(LT,MAP2,REST).


% ----------------------------------------------------------



% ----------------------------------------------------------

/*
There are 7 different clauses to express sc_join_cd_on_EXPR..

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
% IMPORTANT. IMPORTANT: roll back to commit 0ebccc69c58c1c6 to see a 'pure crossing' version with no join conditions




meets_join_sc_cd(
  sc_cd(sc(CART_sc,
           _CART_DATE),
        cd(CART_cd,
           _PRODUCT)) ) :-

        CART_sc = CART_cd.



sc_join_cd_on_EXPR( [], [], [] ).


sc_join_cd_on_EXPR(
  [sc(CART_sc,
           CART_DATE)   |L2T],
  [],
  [] ) :-

        t_ShoppingCart(CART_sc,
                       CART_DATE),

        t_table_content_scart([sc(CART_sc,
           CART_DATE)   |L2T]),

        manageable_list_tail(L2T).


sc_join_cd_on_EXPR(
  [],
  [cd(CART_cd,
           PRODUCT)   |L2T],
  [] ) :-

        t_CartDetail(CART_cd, PRODUCT),

        t_table_content_cdetail([cd(CART_cd,
           PRODUCT)   |L2T]),

        manageable_list_tail(L2T).


% single barcode but longer list of purchase, MEETS JOIN conditions
% ---------- todo ....  EXPERIMENT here... the join condition can be encoded in the HEAD right here, i think
sc_join_cd_on_EXPR(
  [sc(CART_sc,
           CART_DATE)   |[]],
  [cd(CART_cd,
           PRODUCT)   |L2T],
  [sc_cd(sc(CART_sc,
           CART_DATE),
           cd(CART_cd,
           PRODUCT))   |R]  ) :-

        t_ShoppingCart(CART_sc,CART_DATE),

        %t_CartDetail(CART_cd,PRODUCT),

        t_table_content_cdetail([cd(CART_cd,
           PRODUCT)   |L2T]),
        length([cd(CART_cd,
           PRODUCT)   |L2T],X),
        X>1,
        manageable_list_tail(L2T),
        meets_join_sc_cd(sc_cd(sc(CART_sc,
           CART_DATE),
                                   cd(CART_cd,
           PRODUCT))),
        sc_join_cd_on_EXPR( [sc(CART_sc,
           CART_DATE)   |[]], L2T, R ).


% single barcode but longer list of purchase, FAILS TO MEET JOIN conditions
% ---------- todo ....  EXPERIMENT here... the join condition can be encoded in the HEAD right here, i think
sc_join_cd_on_EXPR(
  [sc(CART_sc,
           CART_DATE)   |[]],
  [cd(CART_cd,
           PRODUCT)   |L2T],
  R ) :-

        t_ShoppingCart(CART_sc,CART_DATE),

        t_table_content_cdetail([cd(CART_cd,
           PRODUCT)   |L2T]),
        length([cd(CART_cd,
           PRODUCT)   |L2T],X),
        X>1,
        manageable_list_tail(L2T),
        \+meets_join_sc_cd(sc_cd(sc(CART_sc,
           CART_DATE),
                                 cd(CART_cd,
           PRODUCT))),
        sc_join_cd_on_EXPR( [sc(CART_sc,
           CART_DATE)   |[]], L2T, R ).


% longer barcode list but SINGLE purchase, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [sc(CART_sc,
           CART_DATE)   |L2T],
  [cd(CART_cd,
           PRODUCT)   |[]],
  [sc_cd(sc(CART_sc,
           CART_DATE),
         cd(CART_cd,
           PRODUCT))   |R]  ) :-

        t_table_content_scart(
            [sc(CART_sc,
           CART_DATE)   |L2T]),

        t_CartDetail(CART_cd,PRODUCT),

        manageable_list_tail(L2T),
        meets_join_sc_cd(sc_cd(sc(CART_sc,
           CART_DATE),
                               cd(CART_cd,
           PRODUCT))),
        sc_join_cd_on_EXPR( L2T,
                                [cd(CART_cd,
           PRODUCT)   |[]],
                                R ).


% longer barcode list but SINGLE purchase, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [sc(CART_sc,
           CART_DATE)   |L2T],
  [cd(CART_cd,
           PRODUCT)   |[]],
  R ) :-

        t_table_content_scart(
            [sc(CART_sc,
           CART_DATE)   |L2T]),

        t_CartDetail(CART_cd,PRODUCT),

        manageable_list_tail(L2T),
        \+meets_join_sc_cd(sc_cd(sc(CART_sc,
           CART_DATE),
                                 cd(CART_cd,
           PRODUCT))),
        sc_join_cd_on_EXPR( L2T,
                                [cd(CART_cd,
           PRODUCT)   |[]],
                                R ).


% adding one more purchase to an 'already crossing'
sc_join_cd_on_EXPR(
  [sc(CART_sc,
           CART_DATE)   |L1T], % this list needs to be nonempty. the empty case is handled elsewhere
  [cd(CART_cd,
           PRODUCT)   |L2T],
  FINAL ) :-

        t_table_content_scart(
            [sc(CART_sc,
           CART_DATE)   |L1T]),
        t_table_content_cdetail(
            [cd(CART_cd,
           PRODUCT)   |L2T]),
        length([sc(CART_sc,
           CART_DATE)   |L1T],
               X),
        X>1,
        length([cd(CART_cd,
           PRODUCT)   |L2T],
               Y),
        Y>1,
        X>=Y,
        sc_join_cd_on_EXPR([sc(CART_sc,
           CART_DATE)   |L1T],
                               L2T,
                               POUT),
        sc_join_cd_on_EXPR([sc(CART_sc,
           CART_DATE)   |L1T],
                               [cd(CART_cd,
           PRODUCT)   |[]],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more barcode to an 'already crossing'
sc_join_cd_on_EXPR(
  [sc(CART_sc,
           CART_DATE)   |L1T],
  [cd(CART_cd,
           PRODUCT)  |D], % this list needs to be nonempty. the empty case is handled elsewhere
  FINAL ) :-

        t_table_content_scart([sc(CART_sc,
           CART_DATE)   |L1T]),
        t_table_content_cdetail([cd(CART_cd,
           PRODUCT)   |D]),
        length([sc(CART_sc,
           CART_DATE)   |L1T],
               X),
        X>1,
        length([cd(CART_cd,
           PRODUCT)
   |D],
               Y),
        Y>1,
        X<Y,
        sc_join_cd_on_EXPR(L1T,
                               [cd(CART_cd,
           PRODUCT)   |D],
                               POUT),
        sc_join_cd_on_EXPR([sc(CART_sc,
           CART_DATE)   |[]],
                               [cd(CART_cd,
           PRODUCT)
                      |D],
                               MOUT),
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

appr_corr_join_derived_tuple(
  sc_cd(sc(CART,CART_DATE), cd(CART,PRODUCT)) ) :-

        t_ShoppingCart(CART,CART_DATE),
        t_CartDetail(CART,PRODUCT).

/*
J = [sc_cd(sc(fccy463, 0), cd(fccy463, rural)),
     sc_cd(sc(fccy463, 0), cd(fccy463, noise))]
*/


/*
  note: the final map can be examined with: assoc_to_list, assoc_to_values

  possibly also with failure-driven backtracking:
    gen_assoc(?Key, +Assoc, ?Value)
      Enumerate matching elements of Assoc in ascending order of their keys via backtracking.
*/
appr_corr_group_by(L,LOUT) :-

        appr_corr_group_by(L,t,LOUT). % - the type of L will be asserted inside the 3-arg predicate.


% nothing in the list for further processing. so your 'map so-far' is your finished map.
appr_corr_group_by([],MAP,MAP) :-

        write( '   -----------------------   ' ), nl.



% take the list-of-tuples, our 'so-far' map, and produce a done-map.
appr_corr_group_by(
  [sc_cd(sc(CART_sc,CART_DATE), cd(CART_cd,PRODUCT_gk))   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),
        appr_corr_join_derived_table([sc_cd(sc(CART_sc,CART_DATE), cd(CART_cd,PRODUCT_gk))   |LT]),

/*
        t_AmenitiesAccessBarcode(BARCODE_STRING,
                                 GROUP_KEY, %BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
  */
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
  [sc_cd(sc(CART_sc,CART_DATE), cd(CART_cd,PRODUCT_gk))   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),
        appr_corr_join_derived_table([sc_cd(sc(CART_sc,CART_DATE), cd(CART_cd,PRODUCT_gk))   |LT]),

        \+get_assoc(PRODUCT_gk,MAP,_), % map key (PRODUCT_gk) needs to be instantiated by here.
        put_assoc(PRODUCT_gk,
                  MAP,
                  g(d_prod_cart(CART_DATE,PRODUCT_gk,CART_sc),  % aggregate: min
                    1), % note: 1 is the starting point for the count aggregate
                  MAP2),
        appr_corr_group_by(LT,MAP2,MAP_OUT).




test_it(X,K) :-

        appr_corr_group_by(X,Y), assoc_to_values(Y,K).