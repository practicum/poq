


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).


/*
loop s
  loop p
     throw out this s if we do not find an sp row matching+ sp.s,sp.p
  */


t_Supplier(S_ID_s) :-
        demonat(S_ID_s), nonnull(S_ID_s).


t_SPJoin(
  S_ID_sp,
  P_ID_sp) :-

        demonat(S_ID_sp), nonnull(S_ID_sp),
        demonat(P_ID_sp), nonnull(P_ID_sp).

t_Part(P_ID_p) :-

        demonat(P_ID_p), nonnull(P_ID_p).



% ----------------------------------------------------------

% putting the UNIQUE S_ID_s info here
t_table_content_supplier(L) :-
        % t is the empty mapping, from library assoc
        list_type_sc_removed_dup_supplier(L,t,L).


list_type_sc_removed_dup_supplier([],_ASSOC,[]).


list_type_sc_removed_dup_supplier(
  [s(S_ID_s)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_Supplier(S_ID_s),

        get_assoc(S_ID_s,MAP,_EXISTSVAL), % map key (S_ID_s) needs to be instantiated by here.

        list_type_sc_removed_dup_supplier(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_sc_removed_dup_supplier(
  [s(S_ID_s)   |LT],
  MAP,
  [s(S_ID_s)   |REST]) :-

        manageable_list_tail(LT),
        t_Supplier(S_ID_s),

        \+get_assoc(S_ID_s,MAP,_EXISTSVAL),  % map key (S_ID_s) needs to be instantiated by here.
        put_assoc(S_ID_s,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_sc_removed_dup_supplier(LT,MAP2,REST).



% ----------------------------------------------------------

% putting the UNIQUE P_ID_p information here.
t_table_content_part(L) :-
        % t is the empty mapping, from library assoc
        list_type_sc_removed_dup_part(L,t,L).


list_type_sc_removed_dup_part([],_ASSOC,[]).


list_type_sc_removed_dup_part(
  [p(P_ID_p)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_Part(P_ID_p),

        get_assoc(P_ID_p,MAP,_EXISTSVAL), % map key (P_ID_p) needs to be instantiated by here.

        list_type_sc_removed_dup_part(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_sc_removed_dup_part(
  [p(P_ID_p)   |LT],
  MAP,
  [p(P_ID_p)   |REST]) :-

        manageable_list_tail(LT),
        t_Part(P_ID_p),

        \+get_assoc(P_ID_p,MAP,_EXISTSVAL),  % map key (P_ID_p) needs to be instantiated by here.
        put_assoc(P_ID_p,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_sc_removed_dup_part(LT,MAP2,REST).




% ----------------------------------------------------------

% putting the UNIQUE (S_ID_sp,P_ID_sp) information here.
t_table_content_spjoin(L) :-
        % t is the empty mapping, from library assoc
        list_type_sp_compound_key(L,t,L).


list_type_sp_compound_key([],_ASSOC,[]).


list_type_sp_compound_key(
  [sp(S_ID_sp,P_ID_sp)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_SPJoin(S_ID_sp,P_ID_sp),

        get_assoc(ck(S_ID_sp,P_ID_sp),MAP,_EXISTSVAL), % map key ck(S_ID_sp,P_ID_sp) needs to be instantiated by here.

        list_type_sp_compound_key(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_sp_compound_key(
  [sp(S_ID_sp,P_ID_sp)   |LT],
  MAP,
  [sp(S_ID_sp,P_ID_sp)   |REST]) :-

        manageable_list_tail(LT),
        t_SPJoin(S_ID_sp,P_ID_sp),

        \+get_assoc(ck(S_ID_sp,P_ID_sp),MAP,_EXISTSVAL),  % map key (ck(S_ID_sp,P_ID_sp)) needs to be instantiated by here.
        put_assoc(ck(S_ID_sp,P_ID_sp),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_sp_compound_key(LT,MAP2,REST).


% ----------------------------------------------------------



% ----------------------------------------------------------

/*
There are 7 different clauses to express supp_join_part_on_EXPR..

There should be no duplication in outcomes due to careful management
of when each of the 7 clauses is allowed to be applied.

Each one of the 7 handles a NON-OVERLAPPING subset of cases based on
the SIZE of the first two list variables.

The cases (by size of the two lists) are:

[]    []
1     []
[]    1
1     >1
1+    1     (1+ means 'one or more')
2+    2+  ... and the first list size is greater to or EQUAL to the second
2+    2+  ... and the first list size is LESS THAN the second
*/
% IMPORTANT. IMPORTANT: roll back to commit 0ebccc69c58c1c6 to see a 'pure crossing' version with no join conditions



/*
meets_join_supp_part(
  sc_cd(sc(CART_sc,
           _CART_DATE),
        cd(CART_cd,
           _PRODUCT)) ) :-

        CART_sc = CART_cd.
*/

% --  TODO...  it seems the 'on_EXPR' part will not be needed here...

supp_join_part_on_EXPR( [], [], [] ).


supp_join_part_on_EXPR(
  [s(S_ID_s)   |[]],
  [],
  [] ) :-

        t_Supplier(S_ID_s).


supp_join_part_on_EXPR(
  [],
  [p(P_ID_p)  |[]],
  [] ) :-

        t_Part(P_ID_p).


% single barcode but longer list of purchase, MEETS JOIN conditions
supp_join_part_on_EXPR(
  [s(S_ID_s)   |[]],
  [p(P_ID_p)   |L2T],
  [s_p(s(S_ID_s),p(P_ID_p)) | R] ) :-

        t_Supplier(S_ID_s),

        t_table_content_part([p(P_ID_p)   |L2T]),

        length([p(P_ID_p)   |L2T],X),

        X>1,
        manageable_list_tail(L2T),
%        meets_join_supp_part(sc_cd(sc(CART_sc,           CART_DATE),
%                                   cd(CART_cd,           PRODUCT))),
        supp_join_part_on_EXPR( [s(S_ID_s)   |[]], L2T, R ).

/*
% single barcode but longer list of purchase, FAILS TO MEET JOIN conditions
supp_join_part_on_EXPR(
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
%        \+meets_join_supp_part(sc_cd(sc(CART_sc,           CART_DATE),
%                                 cd(CART_cd,           PRODUCT))),
        supp_join_part_on_EXPR( [sc(CART_sc,
           CART_DATE)   |[]], L2T, R ).
*/



% longer barcode list but SINGLE purchase, MEETS JOIN conditions
supp_join_part_on_EXPR(
  [s(S_ID_s)   |L2T],
  [p(P_ID_p)   |[]],
  [s_p(s(S_ID_s),p(P_ID_p)) | R] ) :-

        t_table_content_supplier( [s(S_ID_s)   |L2T] ),

        t_Part(P_ID_p),

        manageable_list_tail(L2T),
%        meets_join_supp_part(sc_cd(sc(CART_sc,           CART_DATE),
%                               cd(CART_cd,           PRODUCT))),
        supp_join_part_on_EXPR( L2T,
                                [p(P_ID_p)   |[]],
                                R ).

/*
% longer barcode list but SINGLE purchase, FAILS TO MEET JOIN conditions
supp_join_part_on_EXPR(
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
%        \+meets_join_supp_part(sc_cd(sc(CART_sc,           CART_DATE),
%                                 cd(CART_cd,           PRODUCT))),
        supp_join_part_on_EXPR( L2T,
                                [cd(CART_cd,
           PRODUCT)   |[]],
                                R ).
*/

% adding one more purchase to an 'already crossing'
supp_join_part_on_EXPR(
  [s(S_ID_s)   |L1T],% this list needs to be nonempty. the empty case is handled elsewhere
  [p(P_ID_p)   |L2T],
  FINAL ) :-

        t_table_content_supplier([s(S_ID_s)   |L1T]),
        t_table_content_part([p(P_ID_p)   |L2T]),


        length([s(S_ID_s)   |L1T],
               X),
        X>1,
        length([p(P_ID_p)   |L2T],
               Y),
        Y>1,
        X>=Y,
        supp_join_part_on_EXPR([s(S_ID_s)   |L1T],
                               L2T,
                               POUT),
        supp_join_part_on_EXPR([s(S_ID_s)   |L1T],
                               [p(P_ID_p)   |[]],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more barcode to an 'already crossing'
supp_join_part_on_EXPR(
  [s(S_ID_s)   |L1T],
  [p(P_ID_p)   |D],% this list needs to be nonempty. the empty case is handled elsewhere
  FINAL ) :-

        t_table_content_supplier([s(S_ID_s)   |L1T]),
        t_table_content_part([p(P_ID_p)   |D]),

        length([s(S_ID_s)   |L1T],
               X),
        X>1,
        length([p(P_ID_p)   |D],
               Y),
        Y>1,
        X<Y,
        supp_join_part_on_EXPR(L1T,
                               [p(P_ID_p)   |D],
                               POUT),
        supp_join_part_on_EXPR([s(S_ID_s)   |[]],
                               [p(P_ID_p)   |D],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------

