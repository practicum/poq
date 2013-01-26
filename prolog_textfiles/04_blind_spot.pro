

:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).

%:- use_module(modules/dbms/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

/*
axiomatized_query(Employee,Q_RESULT),
member((DPT,_,_),Employee),
\+member((DPT,_,_),Q_RESULT).

length(Employee,3),
axiomatized_query(Employee,Q_RESULT),
member((DPT,_,_),Employee),
\+member((DPT,_,_),Q_RESULT).

  Employee = [ (0, jacob, 850), (0, william, 850), (1, isabella, 4500)],
Q_RESULT = [ (0, jacob, 2)],
DPT = 1 ;


*/

expression_1(SALARY) :-
        SALARY @< 1000.

function_1(SALARY,RESULT) :-
        expression_1(SALARY),
        RESULT is 1.

function_1(SALARY,RESULT) :-
        \+expression_1(SALARY),
        RESULT is 0.

employee_tuple(
  DEPT,
  EMP,
  SALARY) :-

        natural_type(DEPT), not_null(DEPT),
        name_string_type(EMP), not_null(EMP),
        salary_type(SALARY), not_null(SALARY).

employee_tuple_in_order(
  DEPT,
  EMP,
  SALARY,
  PRECEDING_VAL,
  RANK_OF_THIS_TUPLE) :-
        map_natural(DEPT,0,V0),
        map_name(EMP,1,V1),
        map_salary(SALARY,2,V2),
        RANK_OF_THIS_TUPLE is V0 + V1 + V2,
        RANK_OF_THIS_TUPLE @>= PRECEDING_VAL.

% ----------------------------------------------------------


employee_table(L) :-
        % t is the empty mapping, from library assoc
        employee_table_with_constraints(L,t,_,L).

employee_table_with_constraints([],_ASSOC,0,[]).

% Note: 'LT' stands for 'list tail'
employee_table_with_constraints(
  [ (DEPT,EMP,SALARY)  |LT], % axiom will recurse on LT
  MAP,    % map ensures no primary key value is repeated
  MAX,    % the MAX number enforces the arbitrary tuple
          % ordering scheme to avoid producing two equivalent
          % tables such as [(a),(b)] and [(b),(a)]
  [ (DEPT,EMP,SALARY)  |LT2]
  ) :-

        %enforce maximum base-table size
        within_table_size_limit([ (DEPT,EMP,SALARY)  |LT]),
        %enforce tuple type (enforce domain types of each column)
        employee_tuple(DEPT,EMP,SALARY),

        %negation on next line means key is not yet in map
        \+get_assoc((EMP),MAP,_EXISTSVAL),
        put_assoc((EMP),MAP,inmap,MAP2),
        employee_table_with_constraints(LT,MAP2,LT_MAX,LT2),
        employee_tuple_in_order(DEPT,EMP,SALARY,LT_MAX,MAX).


% ----------------------------------------------------------





% ----------------------------------------------------------



required_table_type_for_group_by(T) :-
        employee_table(T).

required_tuple_type_for_group_by(COL_1,COL_2,COL_3) :-
        employee_tuple(COL_1,COL_2,COL_3).

restrict_list_tail_size(T) :-
        within_table_size_limit(T).


group_by(L,LOUT) :-

        % assert the type of the table
        required_table_type_for_group_by(L),
        group_by(L,t,LOUT).


% nothing in the list for further processing. so your 'map
% so-far' is your finished map.
group_by([],MAP,MAP) :-

        write( '   -----------------------   ' ), nl.


/*
 this predicate describes how to take the list-of-tuples, our
 'so-far' map, and produce a done-map.

  in this case, the current row >does< group together with some
  already mapped key.
*/
group_by(
  [(COL_1,COL_2,COL_3)   |LT],
  MAP,
  MAP_OUT ) :-

     restrict_list_tail_size(LT),

     required_tuple_type_for_group_by(COL_1,COL_2,COL_3),

     get_assoc(COL_1,
               MAP,
               (COL_1_SOFAR,COL_2_SOFAR,COL_3_SOFAR)),

     % there should be 1 line of 'agg_field*' statement for each
     % column in the table
     agg_field_do_nothing(COL_1_SOFAR,COL_1,COL_1_AGG),
     agg_field_do_nothing(COL_2_SOFAR,COL_2,COL_2_AGG),
     agg_field_col_three(COL_3_SOFAR,COL_3,COL_3_AGG),

     put_assoc(COL_1,
               MAP,
               (COL_1_AGG,COL_2_AGG,COL_3_AGG),
               MAP2),

     group_by(LT,MAP2,MAP_OUT).

/*
 this predicate describes how to take the list-of-tuples, our
 'so-far' map, and produce a done-map.

  in this case, the group-key for the current row is new
  (never-seen so far), so we put starting values in the map for
  this key.
*/
group_by(
  [(COL_1,COL_2,COL_3)   |LT],
  MAP,
  MAP_OUT ) :-

        restrict_list_tail_size(LT),

        required_tuple_type_for_group_by(COL_1,COL_2,COL_3),

        \+get_assoc(COL_1,MAP,_),

        % there should be 1 line of 'agg_base*' statement for
        % each column in the table
        agg_base_do_nothing(COL_1,COL_1_AGG),
        agg_base_do_nothing(COL_2,COL_2_AGG),
        agg_base_col_three(COL_3,COL_3_AGG),

        put_assoc(COL_1,
                  MAP,
                  (COL_1_AGG,COL_2_AGG,COL_3_AGG),
                  MAP2),
        group_by(LT,MAP2,MAP_OUT).



apply_where_clause([],[]).

apply_where_clause([(DEPT,EMP,SALARY)|L1T],[(DEPT,EMP,SALARY)|L2T]) :-
        natural_type(DEPT),
        name_string_type(EMP),
        salary_type(SALARY),
        expression_1(SALARY),
        apply_where_clause(L1T,L2T).

apply_where_clause([(DEPT,EMP,SALARY)|L1T],L2T) :-
        natural_type(DEPT),
        name_string_type(EMP),
        salary_type(SALARY),
        \+expression_1(SALARY),
        apply_where_clause(L1T,L2T).





/*
agg_field_col_three(COL_3_SOFAR,COL_3,COL_3_AGG) :-
        agg_field_count(COL_3_SOFAR,COL_3,COL_3_AGG).

agg_base_col_three(COL_3,COL_3_AGG) :-
        agg_base_count(COL_3,COL_3_AGG).

% this is query B, which is flawed
axiomatized_query(Employee,Q_RESULT) :-
        employee_table(Employee),
        apply_where_clause(Employee,E2),
        group_by(E2,A),
        assoc_to_values(A,Q_RESULT).
*/


agg_field_col_three(PREVIOUS,INCOMING,WINNER) :-
        function_1(INCOMING,OUT),
        WINNER is PREVIOUS + OUT.

agg_base_col_three(INCOMING,OUTPUT) :-
        function_1(INCOMING,OUTPUT).

% this is query A (good)
axiomatized_query(Employee,Q_RESULT) :-
        group_by(Employee,A), assoc_to_values(A,Q_RESULT).


