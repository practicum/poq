/*
  https://github.com/practicum/poq

  The code below is a template. Filling in the parameters of the
  template will produce usable Prolog axioms.
*/

/*
  Three parameters need filling in each time this template is
  employed:

  ~~t-name~~  :
        fill in with table name in valid Prolog predicate syntax

  #domain-tup-vars#  :
        fill in with comma separated list of domain variables,
        one variable per tuple-component according to the
        tuple structure of the rows of this base-table.
        names should be valid Prolog variable names.
        for example:  CART_ID, CART_DATE, CUSTOMER_ID

  #key#  :
        fill in with comma separated list of only those
        domain variables that match the column (or columns)
        of the tables primary key.
*/

~~t-name~~_table(L) :-
        % t is the empty mapping, from library assoc
        ~~t-name~~_table_with_constraints(L,t,_,L).

~~t-name~~_table_with_constraints([],_ASSOC,0,[]).

% Note: 'LT' stands for 'list tail'
~~t-name~~_table_with_constraints(
  [ (#domain-tup-vars#)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (#domain-tup-vars#)  |LT2]
  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (#domain-tup-vars#)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        ~~t-name~~_tuple(#domain-tup-vars#),

        %negation on next line means key is not yet in map
        \+get_assoc((#key#),MAP,_EXISTSVAL),
        put_assoc((#key#),MAP,inmap,MAP2),
        ~~t-name~~_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        ~~t-name~~_tuple_in_order(#domain-tup-vars#,LT_MAX,MAX).