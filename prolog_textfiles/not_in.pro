


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).





t_TypeOne(T01) :-
        demonat(T01).


t_TypeTwo(T02) :-
        demonat(T02), nonnull(T02).




% ----------------------------------------------------------

% putting the UNIQUE  info here
t_table_content_type_1(L) :-
        % t is the empty mapping, from library assoc
        list_type_sc_removed_dup_type_1(L,t,L).


list_type_sc_removed_dup_type_1([],_ASSOC,[]).


list_type_sc_removed_dup_type_1(
  [t1(T01)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_TypeOne(T01),

        get_assoc(T01,MAP,_EXISTSVAL), % map key (T01) needs to be instantiated by here.

        list_type_sc_removed_dup_type_1(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_sc_removed_dup_type_1(
  [t1(T01)   |LT],
  MAP,
  [t1(T01)   |REST]) :-

        manageable_list_tail(LT),
        t_TypeOne(T01),

        \+get_assoc(T01,MAP,_EXISTSVAL),  % map key (T01) needs to be instantiated by here.
        put_assoc(T01,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_sc_removed_dup_type_1(LT,MAP2,REST).



% ----------------------------------------------------------

% putting the UNIQUE  information here.
t_table_content_type_2(L) :-
        % t is the empty mapping, from library assoc
        list_type_sc_removed_dup_type_2(L,t,L).


list_type_sc_removed_dup_type_2([],_ASSOC,[]).


list_type_sc_removed_dup_type_2(
  [t2(T02)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_TypeTwo(T02),

        get_assoc(T02,MAP,_EXISTSVAL), % map key (T02) needs to be instantiated by here.

        list_type_sc_removed_dup_type_2(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_sc_removed_dup_type_2(
  [t2(T02)   |LT],
  MAP,
  [t2(T02)   |REST]) :-

        manageable_list_tail(LT),
        t_TypeTwo(T02),

        \+get_assoc(T02,MAP,_EXISTSVAL),  % map key (P_ID_p) needs to be instantiated by here.
        put_assoc(T02,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_sc_removed_dup_type_2(LT,MAP2,REST).




/*
  T1 - the actual TypeOne table for this instance of the scenario.
  T2 - the actual TypeTwo table for this instance of the scenario.

  QR - result of query when performed on the above-mentioned objects (from 'null cannot not in' pitfall)
 */
run_query_not_exists(T1,T2,QR) :-

        %t_table_content_type_1(T1),
        %t_table_content_type_2(T2),

        filter_notexists(T1,T2,T1,QR).

filter_notexists([],_T2_CONST,T1_SUB,T1_SUB) :- % could check types

        write( '   -----------------------   ' ), nl.

/*
  We need to recurse down through the whole MOD_T1.

  T2_CONST will stay the same throughout.

  T1_SUBSET needs to 'begin' (start out?) holding exactly the content of T1.

  - need the current head of MOD_T1.
  - the current head has 't'.
  - if t is NOT in T2_CONST, then *delete* 't' from T1_SUBSET
  - otherwise, leave T1_SUBSET unchanged and recurse on MOD_T1 tail
*/


filter_notexists([t1(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        \+member(t2(T01) ,T2_CONST), % - if t is not in T2_CONST, then keep it
        filter_notexists(T1_T,T2_CONST,T1_SUBSET,NEXT_SUBSET).%- otherwise, leave STAB_SUBSET unchanged and recurse on MOD_XSP tail

filter_notexists([t1(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        member(t2(T01) ,T2_CONST), % - when we find t in T2_CONST, then delete it from our result
        delete(T1_SUBSET,t1(T01),NEW_SUBSET), % TODO -check behavior: if s is NOT in STAB. if s is in there twice?
        filter_notexists(T1_T,T2_CONST,NEW_SUBSET,NEXT_SUBSET).







/*
  T1 - the actual TypeOne table for this instance of the scenario.
  T2 - the actual TypeTwo table for this instance of the scenario.

  QR - result of query when performed on the above-mentioned objects (from 'null cannot not in' pitfall)
 */
run_query_not_in(T1,T2,QR) :-

        %t_table_content_type_1(T1),
        %t_table_content_type_2(T2),

        filter_notin(T1,T2,T1,QR).

filter_notin([],_T2_CONST,T1_SUB,T1_SUB) :- % could check types

        write( '   -----------------------   ' ), nl.

/*
  We need to recurse down through the whole MOD_T1.

  T2_CONST will stay the same throughout.

  T1_SUBSET needs to 'begin' (start out?) holding exactly the content of T1.

  - need the current head of MOD_T1.
  - the current head has 't'.
  - if t is NOT in T2_CONST, then *delete* 't' from T1_SUBSET
  - otherwise, leave T1_SUBSET unchanged and recurse on MOD_T1 tail
*/


filter_notin([t1(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        \+member(t2(T01) ,T2_CONST), % - if t is not in T2_CONST, then keep it
        nonnull(T01),
        filter_notin(T1_T,T2_CONST,T1_SUBSET,NEXT_SUBSET).%- otherwise, leave STAB_SUBSET unchanged and recurse on MOD_XSP tail




filter_notin([t1(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        T01 = null, % filter out 'null' from the results
        delete(T1_SUBSET,t1(T01),NEW_SUBSET), % TODO -check behavior: if s is NOT in STAB. if s is in there twice?
        filter_notin(T1_T,T2_CONST,NEW_SUBSET,NEXT_SUBSET).


filter_notin([t1(T01)|T1_T], % MOD_T1,
            T2_CONST,
            T1_SUBSET,
            NEXT_SUBSET) :-

        member(t2(T01) ,T2_CONST), % - when we find t in T2_CONST, then delete it from our result
        delete(T1_SUBSET,t1(T01),NEW_SUBSET), % TODO -check behavior: if s is NOT in STAB. if s is in there twice?
        filter_notin(T1_T,T2_CONST,NEW_SUBSET,NEXT_SUBSET).






