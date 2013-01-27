/*
  https://github.com/practicum/poq

  The code below is a template. Filling in the parameters of the
  template will produce usable Prolog axioms.
*/

/*
  Three parameters need filling in each time this template is
  employed:

  ~~filter-name~~  :
        fill in with any unique filter name in valid Prolog
        predicate syntax

  ~~table-type-check~~  :
        fill in with the appropriate table type-enforcement
        predicate (which will be an intance of the template in
        template_table.pro)

  #domain-tup-vars#  :
        fill in with comma separated list of domain variables,
        one variable per tuple-component according to the
        tuple structure of the rows of the table operand.
        names should be valid Prolog variable names.
        for example:  CART_ID, CART_DATE, CUSTOMER_ID
*/



meets_criteria_~~filter-name~~(#domain-tup-vars#) :-
        % logical formula for criteria goes here.

filter_list_~~filter-name~~([],[]).

% Note: 'LT' stands for 'list tail'
filter_list_~~filter-name~~(
  [(#domain-tup-vars#)|LT],
  [(#domain-tup-vars#)|LT2]) :-

        ~~table-type-check~~([(#domain-tup-vars#)|LT]),
        meets_criteria_~~filter-name~~(#domain-tup-vars#),
        filter_list_~~filter-name~~(LT,LT2).

filter_list_~~filter-name~~(
  [(#domain-tup-vars#)|LT],
  LT2) :-

        ~~table-type-check~~([(#domain-tup-vars#)|LT]),
        \+meets_criteria_~~filter-name~~(#domain-tup-vars#),
        filter_list_~~filter-name~~(LT,LT2).

