
/*
  tell SWIPL to treat files ending in extension '.pro' as Prolog
  source files.

  Note: 'prolog_file_type' is a dynamic multifile predicate
  defined in module user.
*/
user:prolog_file_type(pro,prolog).

?- use_module(modules/dbms/mini_datatypes).
?- use_module(modules/dbms/small_lists).
?- use_module(modules/dbms/dbms_builtins).

?- ['04_blind_spot'].


agg_field_col_three(COL_3_SOFAR,COL_3,COL_3_AGG) :-
        agg_field_count(COL_3_SOFAR,COL_3,COL_3_AGG).

agg_base_col_three(COL_3,COL_3_AGG) :-
        agg_base_count(COL_3,COL_3_AGG).

% this is query B, which is flawed
axiomatized_query(Employee,Q_RESULT) :-
        employee_table(Employee),
        filter_list_where_clause(Employee,E2),
        group_by(E2,A),
        assoc_to_values(A,Q_RESULT).


main :-

        % the output seems more 'interesting' with more content
        % in Employee:
        length(Employee,3),

        % begin proof task as shown in thesis manuscript
        axiomatized_query(Employee,Q_RESULT),
        member((DPT,_,_),Employee),
        \+member((DPT,_,_),Q_RESULT),
        % end proof task as shown in thesis manuscript

        writeln('Employee ='),
        write(Employee),nl,
        writeln('Q_RESULT ='),
        write(Q_RESULT),nl.


% if we get here, then the earlier clause for 'main' failed to be
% satisfied, which means that no counterexamples exist.
main :-
  writeln(
    'Code is verified correct with respect to its specification.').

