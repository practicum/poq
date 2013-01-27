
/*
  tell SWIPL to treat files ending in extension '.pro' as Prolog
  source files.

  Note: 'prolog_file_type' is a dynamic multifile predicate
  defined in module user.
*/
user:prolog_file_type(pro,prolog).

?- use_module(modules/dbms/mini_datatypes).
?- use_module(modules/dbms/small_lists).

?- ['06_eq_empty_pie'].


main :-
        % assert the precondition
        non_empty_table(Part),

        % begin proof task as shown in thesis manuscript
        axiomatized_query(
           Supplier,Part,SPJoin,Q_RESULT),
        member( (SID), Q_RESULT ),
        \+member( (SID,_), SPJoin ),
        % end proof task as shown in thesis manuscript

        writeln('Supplier ='),
        write(Supplier),nl,
        writeln('Part ='),
        write(Part),nl,
        writeln('SPJoin ='),
        write(SPJoin),nl,
        writeln('Q_RESULT ='),
        write(Q_RESULT),nl.


% if we get here, then the earlier clause for 'main' failed to be
% satisfied, which means that no counterexamples exist.
main :-
  writeln(
    'Code is verified correct with respect to its specification.').

