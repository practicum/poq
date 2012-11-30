


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
There are 7 different clauses to express supp_cross_part.

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

supp_cross_part( [], [], [] ).


supp_cross_part(
  [s(S_ID_s)   |[]],
  [],
  [] ) :-

        t_Supplier(S_ID_s).


supp_cross_part(
  [],
  [p(P_ID_p)  |[]],
  [] ) :-

        t_Part(P_ID_p).


% single barcode but longer list of purchase, MEETS JOIN conditions
supp_cross_part(
  [s(S_ID_s)   |[]],
  [p(P_ID_p)   |L2T],
  [s_p(s(S_ID_s),p(P_ID_p)) | R] ) :-

        t_Supplier(S_ID_s),

        t_table_content_part([p(P_ID_p)   |L2T]),

        length([p(P_ID_p)   |L2T],X),

        X>1,
        manageable_list_tail(L2T),
        supp_cross_part( [s(S_ID_s)   |[]], L2T, R ).



% longer barcode list but SINGLE purchase, MEETS JOIN conditions
supp_cross_part(
  [s(S_ID_s)   |L2T],
  [p(P_ID_p)   |[]],
  [s_p(s(S_ID_s),p(P_ID_p)) | R] ) :-

        t_table_content_supplier( [s(S_ID_s)   |L2T] ),

        t_Part(P_ID_p),

        manageable_list_tail(L2T),
        supp_cross_part( L2T,
                                [p(P_ID_p)   |[]],
                                R ).



% adding one more purchase to an 'already crossing'
supp_cross_part(
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
        supp_cross_part([s(S_ID_s)   |L1T],
                               L2T,
                               POUT),
        supp_cross_part([s(S_ID_s)   |L1T],
                               [p(P_ID_p)   |[]],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more barcode to an 'already crossing'
supp_cross_part(
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
        supp_cross_part(L1T,
                               [p(P_ID_p)   |D],
                               POUT),
        supp_cross_part([s(S_ID_s)   |[]],
                               [p(P_ID_p)   |D],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------

/*
  STAB - the actual Supplier table for this instance of the scenario.
  PTAB - the actual Part table for this instance of the scenario.
  XSP - a cartesian crossing of STAB and PTAB. (equivalent to nested loops: for s in Supp { for p in Part })

  SPJ - the actual SPJoin table. (IMPORTANT WARNING: not foreign-key constrained!)

  QR - result of query when performed on the above-mentioned objects (from example one of equal shares of an empty pie)


 */
run_query(STAB,PTAB,XSP,SPJ,QR) :-

        supp_cross_part(STAB,PTAB,XSP),
        t_table_content_spjoin(SPJ),

        filter_supp(SPJ,XSP,STAB,QR).


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
            [s_p(s(S_ID_s), p(P_ID_p))|XSP_T], %MOD_XSP,
            STAB_SUBSET,
            NEXT_SUBSET) :-

        \+member(sp(S_ID_s,P_ID_p) ,SPJ_CONST), % - if s,p is NOT in SPJ_CONST, then *delete* 's' from STAB_SUBSET
        delete(STAB_SUBSET,s(S_ID_s),NEW_SUBSET), % TODO -check behavior: if s is NOT in STAB. if s is in there twice?
        filter_supp(SPJ_CONST,XSP_T,NEW_SUBSET,NEXT_SUBSET).

filter_supp(SPJ_CONST,
            [s_p(s(S_ID_s), p(P_ID_p))|XSP_T], %MOD_XSP,
            STAB_SUBSET,
            NEXT_SUBSET) :-

        member(sp(S_ID_s,P_ID_p) ,SPJ_CONST),
        filter_supp(SPJ_CONST,XSP_T,STAB_SUBSET,NEXT_SUBSET).%- otherwise, leave STAB_SUBSET unchanged and recurse on MOD_XSP tail




