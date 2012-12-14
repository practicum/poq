
% putting the UNIQUE barcode_string information here.
% TODO: what if two columns bore the unique keyword?
~~table-name~~_table(L) :-
        % t is the empty mapping, from library assoc
        ~~table-name~~_table_with_constraints(L,t,_,L).


~~table-name~~_table_with_constraints([],_ASSOC,0,[]).


~~table-name~~_table_with_constraints(
  [(#SS1#)   |LT],
  MAP,
  CURR_MAX,
  [(#SS1#)   |REST]) :-

        within_table_size_limit([(#SS1#)   |LT]),
        ~~table-name~~_tuple(#SS1#),

        \+get_assoc((#key#),MAP,_EXISTSVAL),  % map key needs to be instantiated by here.
        put_assoc((#key#),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        ~~table-name~~_table_with_constraints(LT,MAP2,LT_MAX,REST),
        ~~table-name~~_tuple_in_order(#SS1#,LT_MAX,CURR_MAX).

