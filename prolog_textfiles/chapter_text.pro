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
product_string_type(guaifenesin).


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






