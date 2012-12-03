/*
  File: chapter_text.pro

  */


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

% remember: we are talking about X should be INSTANTIATED ALREADY
not_null(X) :- \+isnull(X).


product_string_type(null).
product_string_type(aspirin).
product_string_type(ibuprofen).
%product_string_type(guaifenesin).


product_tuple( PROD_ID, PROD_NAME ) :-
    natural_type(PROD_ID), not_null(PROD_ID),% remember: we are talking about X should be INSTANTIATED ALREADY
    product_string_type(PROD_NAME).


/*
%  this was the initial 'first pass' at defining the Product table


product_table( [] ).

product_table( [(P_ID,P_NAME)|T] ) :-

        within_table_size_limit([(P_ID,P_NAME)|T]),
        product_tuple(P_ID, P_NAME),
        product_table(T).

*/



product_table(T) :-
        % t is the empty mapping, from library assoc
        product_table_with_constraints(T,t,T).


product_table_with_constraints([],_ASSOC,[]).


product_table_with_constraints(
  [(P_ID,P_NAME)  |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        product_tuple(P_ID,P_NAME),
        get_assoc(P_ID,MAP,_EXISTSVAL), % map key (P_ID) needs to be instantiated by here.
        product_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


product_table_with_constraints(
  [(P_ID,P_NAME)   |LT],
  MAP,
  [(P_ID,P_NAME)   |REST]) :-

        within_table_size_limit([(P_ID,P_NAME)|LT]),
        product_tuple(P_ID,P_NAME),
        \+get_assoc(P_ID,MAP,_EXISTSVAL),  % map key (S_ID_s) needs to be instantiated by here.
        put_assoc(P_ID,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        product_table_with_constraints(LT,MAP2,REST).




% =====================================================================

shopping_cart_tuple(
  CART_ID,
  CUST_ID) :-

        guid_type(CART_ID), not_null(CART_ID),
        natural_type(CUST_ID).


cart_detail_tuple(
  CART_ID,
  PRODUCT) :-

        guid_type(CART_ID), not_null(CART_ID),
        product_string_type(PRODUCT), not_null(PRODUCT).


shopping_cart_table(T) :-
        % t is the empty mapping, from library assoc
        shopping_cart_table_with_constraints(T,t,T).


shopping_cart_table_with_constraints([],_ASSOC,[]).


shopping_cart_table_with_constraints(
  [(CART_ID,CUST_ID)   |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        shopping_cart_tuple(CART_ID,CUST_ID),

        get_assoc(CART_ID,MAP,_EXISTSVAL), % map key (CART_sc) needs to be instantiated by here.

        shopping_cart_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


shopping_cart_table_with_constraints(
  [(CART_ID,CUST_ID)   |LT],
  MAP,
  [(CART_ID,CUST_ID)   |REST]) :-

        within_table_size_limit([(CART_ID,CUST_ID)   |LT]),
        shopping_cart_tuple(CART_ID,CUST_ID),

        \+get_assoc(CART_ID,MAP,_EXISTSVAL),  % map key (CART_ID) needs to be instantiated by here.
        put_assoc(CART_ID,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        shopping_cart_table_with_constraints(LT,MAP2,REST).




% ----------------------------------------------------------


cart_detail_table(L) :-
        % t is the empty mapping, from library assoc
        cart_detail_table_with_constraints(L,t,L).


cart_detail_table_with_constraints([],_ASSOC,[]).


cart_detail_table_with_constraints(
  [(CART_ID, PRODUCT)    |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        cart_detail_tuple(CART_ID, PRODUCT),

        get_assoc(ck(CART_ID, PRODUCT),MAP,_EXISTSVAL), % map key (CART_cd) needs to be instantiated by here.

        cart_detail_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


cart_detail_table_with_constraints(
  [(CART_ID, PRODUCT)    |LT],
  MAP,
  [(CART_ID, PRODUCT)    |REST]) :-

        within_table_size_limit([(CART_ID, PRODUCT)    |LT]),
        cart_detail_tuple(CART_ID, PRODUCT),

        \+get_assoc(ck(CART_ID, PRODUCT),MAP,_EXISTSVAL),  % map key (ck(CART_ID, PRODUCT)) needs to be instantiated by here.
        put_assoc(ck(CART_ID, PRODUCT),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
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

meets_join_sc_cd(CART_ID_sc,_CUST_ID,CART_ID_cd,_PRODUCT) :-
        CART_ID_sc = CART_ID_cd.


sc_join_cd_on_EXPR( [], [], [] ).


sc_join_cd_on_EXPR(
  [(CART_ID_sc,CUST_ID) |L2T],
  [],
  [] ) :-

        shopping_cart_table([(CART_ID_sc,CUST_ID)   |L2T]),

        within_table_size_limit([(CART_ID_sc,CUST_ID)   |L2T]).


sc_join_cd_on_EXPR(
  [],
  [(CART_ID_cd,PRODUCT)   |L2T],
  [] ) :-

        cart_detail_table([(CART_ID_cd,PRODUCT)   |L2T]),

        within_table_size_limit([(CART_ID_cd,PRODUCT)   |L2T]).



% single cart but longer list of c_details, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_ID_sc,CUST_ID)   |[]],

  [(CART_ID_cd,PRODUCT)   |L2T],

  [(CART_ID_sc,CUST_ID,CART_ID_cd,PRODUCT) |R] ) :-


        shopping_cart_tuple(CART_ID_sc,CUST_ID),

        cart_detail_table([(CART_ID_cd,PRODUCT)   |L2T]),

        length([(CART_ID_cd,PRODUCT)   |L2T],X),
        X>1,
        within_table_size_limit(L2T),

        meets_join_sc_cd(CART_ID_sc,CUST_ID,CART_ID_cd,PRODUCT),

        sc_join_cd_on_EXPR([(CART_ID_sc,CUST_ID)   |[]] , L2T, R ).


% single cart but longer list of c_details, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_ID_sc,CUST_ID)   |[]],

  [(CART_ID_cd,PRODUCT)   |L2T],

  R ) :-

        shopping_cart_tuple(CART_ID_sc,CUST_ID),

        cart_detail_table([(CART_ID_cd,PRODUCT)   |L2T]),

        length([(CART_ID_cd,PRODUCT)   |L2T],X),

        X>1,
        within_table_size_limit(L2T),

        \+meets_join_sc_cd(CART_ID_sc,CUST_ID,CART_ID_cd,PRODUCT),

        sc_join_cd_on_EXPR([(CART_ID_sc,CUST_ID)   |[]] , L2T, R ).



% longer carts list but SINGLE detail item, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_ID_sc,CUST_ID)   |L2T],

  [(CART_ID_cd,PRODUCT)   |[]],

  [(CART_ID_sc,CUST_ID,CART_ID_cd,PRODUCT) |R] ) :-


        shopping_cart_table([(CART_ID_sc,CUST_ID)   |L2T]),

        cart_detail_tuple(CART_ID_cd,PRODUCT),

        within_table_size_limit(L2T),

        meets_join_sc_cd(CART_ID_sc,CUST_ID,CART_ID_cd,PRODUCT),

        sc_join_cd_on_EXPR( L2T, [(CART_ID_cd,PRODUCT)   |[]] ,   R ).


% longer carts list but SINGLE details item, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [(CART_ID_sc,CUST_ID)   |L2T],

  [(CART_ID_cd,PRODUCT)   |[]],

  R ) :-

        shopping_cart_table([(CART_ID_sc,CUST_ID)   |L2T]),

        cart_detail_tuple(CART_ID_cd,PRODUCT),

        within_table_size_limit(L2T),

        \+meets_join_sc_cd(CART_ID_sc,CUST_ID,CART_ID_cd,PRODUCT),

        sc_join_cd_on_EXPR( L2T, [(CART_ID_cd,PRODUCT)   |[]] ,   R ).



% adding one more details item to an 'already crossing'
sc_join_cd_on_EXPR(
  [(CART_ID_sc,CUST_ID)   |L1T],

  [(CART_ID_cd,PRODUCT)   |L2T],

  FINAL ) :-

        shopping_cart_table([(CART_ID_sc,CUST_ID)   |L1T]),

        cart_detail_table([(CART_ID_cd,PRODUCT)   |L2T]),

        length([(CART_ID_sc,CUST_ID)   |L1T],  X),
        X>1,
        length([(CART_ID_cd,PRODUCT)   |L2T],  Y),
        Y>1,
        X>=Y,
        sc_join_cd_on_EXPR([(CART_ID_sc,CUST_ID)   |L1T],    L2T,      POUT),
        sc_join_cd_on_EXPR([(CART_ID_sc,CUST_ID)   |L1T], [(CART_ID_cd,PRODUCT)   |[]],  MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more cart to an 'already crossing'
sc_join_cd_on_EXPR(
  [(CART_ID_sc,CUST_ID)   |L1T],

  [(CART_ID_cd,PRODUCT)   |L2T],

  FINAL ) :-

        shopping_cart_table([(CART_ID_sc,CUST_ID)   |L1T]),

        cart_detail_table([(CART_ID_cd,PRODUCT)   |L2T]),

        length([(CART_ID_sc,CUST_ID)   |L1T],  X),
        X>1,
        length([(CART_ID_cd,PRODUCT)   |L2T],  Y),

        Y>1,
        X<Y,
        sc_join_cd_on_EXPR(L1T,  [(CART_ID_cd,PRODUCT)   |L2T],   POUT),
        sc_join_cd_on_EXPR([(CART_ID_sc,CUST_ID)   |[]],   [(CART_ID_cd,PRODUCT)   |L2T],    MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------



