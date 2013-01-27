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



% =====================================================================

table_one_tuple(
  ONE_ID) :-

        natural_type(ONE_ID), not_null(ONE_ID).



table_two_tuple(
  ONE_ID,
  PSTR) :-

        natural_type(ONE_ID), not_null(ONE_ID),
        product_string_type(PSTR), not_null(PSTR).


table_one_table(T) :-
        % t is the empty mapping, from library assoc
        table_one_table_with_constraints(T,t,T).


table_one_table_with_constraints([],_ASSOC,[]).


table_one_table_with_constraints(
  [(ONE_ID)   |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        table_one_tuple(ONE_ID),

        get_assoc(ONE_ID,MAP,_EXISTSVAL), % map key (CART_one) needs to be instantiated by here.

        table_one_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


table_one_table_with_constraints(
  [(ONE_ID)   |LT],
  MAP,
  [(ONE_ID)   |REST]) :-

        within_table_size_limit([(ONE_ID)   |LT]),
        table_one_tuple(ONE_ID),

        \+get_assoc(ONE_ID,MAP,_EXISTSVAL),  % map key (ONE_ID) needs to be instantiated by here.
        put_assoc(ONE_ID,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        table_one_table_with_constraints(LT,MAP2,REST).




% ----------------------------------------------------------


table_two_table(L) :-
        % t is the empty mapping, from library assoc
        table_two_table_with_constraints(L,t,L).


table_two_table_with_constraints([],_ASSOC,[]).


table_two_table_with_constraints(
  [(ONE_ID, PSTR)    |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),
        table_two_tuple(ONE_ID, PSTR),

        get_assoc(ck(ONE_ID, PSTR),MAP,_EXISTSVAL), % map key (CART_cd) needs to be instantiated by here.

        table_two_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


table_two_table_with_constraints(
  [(ONE_ID, PSTR)    |LT],
  MAP,
  [(ONE_ID, PSTR)    |REST]) :-

        within_table_size_limit([(ONE_ID, PSTR)    |LT]),
        table_two_tuple(ONE_ID, PSTR),

        \+get_assoc(ck(ONE_ID, PSTR),MAP,_EXISTSVAL),  % map key (ck(ONE_ID, PSTR)) needs to be instantiated by here.
        put_assoc(ck(ONE_ID, PSTR),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        table_two_table_with_constraints(LT,MAP2,REST).


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

meets_join(ONE_ID_one,ONE_ID_two,_PSTR) :-
        ONE_ID_one = ONE_ID_two.


sc_join_cd_on_EXPR( [], [], [] ).


sc_join_cd_on_EXPR(
  [(ONE_ID_one) |L2T],
  [],
  [] ) :-

        table_one_table([(ONE_ID_one)   |L2T]),

        within_table_size_limit([(ONE_ID_one)   |L2T]).


sc_join_cd_on_EXPR(
  [],
  [(ONE_ID_two,PSTR)   |L2T],
  [] ) :-

        table_two_table([(ONE_ID_two,PSTR)   |L2T]),

        within_table_size_limit([(ONE_ID_two,PSTR)   |L2T]).



% single cart but longer list of c_details, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [(ONE_ID_one)   |[]],

  [(ONE_ID_two,PSTR)   |L2T],

  [(ONE_ID_one,ONE_ID_two,PSTR) |R] ) :-


        table_one_tuple(ONE_ID_one),

        table_two_table([(ONE_ID_two,PSTR)   |L2T]),

        length([(ONE_ID_two,PSTR)   |L2T],X),
        X>1,
        within_table_size_limit(L2T),

        meets_join(ONE_ID_one,ONE_ID_two,PSTR),

        sc_join_cd_on_EXPR([(ONE_ID_one)   |[]] , L2T, R ).


% single cart but longer list of c_details, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [(ONE_ID_one)   |[]],

  [(ONE_ID_two,PSTR)   |L2T],

  R ) :-

        table_one_tuple(ONE_ID_one),

        table_two_table([(ONE_ID_two,PSTR)   |L2T]),

        length([(ONE_ID_two,PSTR)   |L2T],X),

        X>1,
        within_table_size_limit(L2T),

        \+meets_join(ONE_ID_one,ONE_ID_two,PSTR),

        sc_join_cd_on_EXPR([(ONE_ID_one)   |[]] , L2T, R ).



% longer carts list but SINGLE detail item, MEETS JOIN conditions
sc_join_cd_on_EXPR(
  [(ONE_ID_one)   |L2T],

  [(ONE_ID_two,PSTR)   |[]],

  [(ONE_ID_one,ONE_ID_two,PSTR) |R] ) :-


        table_one_table([(ONE_ID_one)   |L2T]),

        table_two_tuple(ONE_ID_two,PSTR),

        within_table_size_limit(L2T),

        meets_join(ONE_ID_one,ONE_ID_two,PSTR),

        sc_join_cd_on_EXPR( L2T, [(ONE_ID_two,PSTR)   |[]] ,   R ).


% longer carts list but SINGLE details item, FAILS TO MEET JOIN conditions
sc_join_cd_on_EXPR(
  [(ONE_ID_one)   |L2T],

  [(ONE_ID_two,PSTR)   |[]],

  R ) :-

        table_one_table([(ONE_ID_one)   |L2T]),

        table_two_tuple(ONE_ID_two,PSTR),

        within_table_size_limit(L2T),

        \+meets_join(ONE_ID_one,ONE_ID_two,PSTR),

        sc_join_cd_on_EXPR( L2T, [(ONE_ID_two,PSTR)   |[]] ,   R ).



% adding one more details item to an 'already crossing'
sc_join_cd_on_EXPR(
  [(ONE_ID_one)   |L1T],

  [(ONE_ID_two,PSTR)   |L2T],

  FINAL ) :-

        table_one_table([(ONE_ID_one)   |L1T]),

        table_two_table([(ONE_ID_two,PSTR)   |L2T]),

        length([(ONE_ID_one)   |L1T],  X),
        X>1,
        length([(ONE_ID_two,PSTR)   |L2T],  Y),
        Y>1,
        X>=Y,
        sc_join_cd_on_EXPR([(ONE_ID_one)   |L1T],    L2T,      POUT),
        sc_join_cd_on_EXPR([(ONE_ID_one)   |L1T], [(ONE_ID_two,PSTR)   |[]],  MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more cart to an 'already crossing'
sc_join_cd_on_EXPR(
  [(ONE_ID_one)   |L1T],

  [(ONE_ID_two,PSTR)   |L2T],

  FINAL ) :-

        table_one_table([(ONE_ID_one)   |L1T]),

        table_two_table([(ONE_ID_two,PSTR)   |L2T]),

        length([(ONE_ID_one)   |L1T],  X),
        X>1,
        length([(ONE_ID_two,PSTR)   |L2T],  Y),

        Y>1,
        X<Y,
        sc_join_cd_on_EXPR(L1T,  [(ONE_ID_two,PSTR)   |L2T],   POUT),
        sc_join_cd_on_EXPR([(ONE_ID_one)   |[]],   [(ONE_ID_two,PSTR)   |L2T],    MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------



