
/*
  tell SWIPL to treat files ending in extension '.pro' as Prolog
  source files.

  Note: 'prolog_file_type' is a dynamic multifile predicate
  defined in module user.
*/
user:prolog_file_type(pro,prolog).

?- use_module(modules/dbms/mini_datatypes).
?- use_module(modules/dbms/small_lists).

?- ['05_not_in'].

axiomatized_query(EntityA,EntityB,X) :-
        run_query_not_exists(EntityA,EntityB,X).

main :-
        % the output seems more 'interesting' with more content
        % in EntityA and EntityB:
        length(EntityA,3),
        length(EntityB,1),

        % begin proof task as shown in thesis manuscript
        axiomatized_query(EntityA,EntityB,Q_RESULT),
        member(null,Q_RESULT),
        % end proof task as shown in thesis manuscript

        writeln('EntityA ='),
        write(EntityA),nl,
        writeln('EntityB ='),
        write(EntityB),nl,
        writeln('Q_RESULT ='),
        write(Q_RESULT),nl.


% if we get here, then the earlier clause for 'main' failed to be
% satisfied, which means that no counterexamples exist.
main :-
  writeln(
    'Code is verified correct with respect to its specification.').

