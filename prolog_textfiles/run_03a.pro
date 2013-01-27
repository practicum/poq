
/*
  tell SWIPL to treat files ending in extension '.pro' as Prolog
  source files.

  Note: 'prolog_file_type' is a dynamic multifile predicate
  defined in module user.
*/
user:prolog_file_type(pro,prolog).

?- use_module(modules/dbms/mini_datatypes).
?- use_module(modules/dbms/small_lists).

?- ['03_left_join'].


meets_join(   PID,PID2,TITLE   ) :-
        PID = PID2,
        expression_1(TITLE).

% this is QD, which is flawed
axiomatized_query(Person,ExtraInfo,Q_RESULT) :-
        left_join_on_expression(Person,ExtraInfo,Q_RESULT).


main :-

        % the output seems more 'interesting' with more content
        % in Person and ExtraInfo:
        length(Person,3),
        length(ExtraInfo,2),

        % begin proof task as shown in thesis manuscript
        axiomatized_query(Person,ExtraInfo,Q_RESULT),
        member( (ID,_,_), Q_RESULT ),
        member( (ID,mr), ExtraInfo ),
        % end proof task as shown in thesis manuscript

        writeln('Person ='),
        write(Person),nl,
        writeln('ExtraInfo ='),
        write(ExtraInfo),nl,
        writeln('Q_RESULT ='),
        write(Q_RESULT),nl.


% if we get here, then the earlier clause for 'main' failed to be
% satisfied, which means that no counterexamples exist.
main :-
  writeln(
    'Code is verified correct with respect to its specification.').

