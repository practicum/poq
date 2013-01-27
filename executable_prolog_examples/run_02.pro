
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

?- ['02_appr_corr'].

main :-
        % begin proof task as shown in thesis manuscript
        axiomatized_query(ShoppingCart,CartDetail,Q_RESULT),
        member( (C,D,_,_), Q_RESULT),
        \+member( (C,D), ShoppingCart ),
        % end proof task as shown in thesis manuscript

        writeln('ShoppingCart ='),
        write(ShoppingCart),nl,
        writeln('CartDetail ='),
        write(CartDetail),nl,
        writeln('Q_RESULT ='),
        write(Q_RESULT),nl.


% if we get here, then the earlier clause for 'main' failed to be
% satisfied, which means that no counterexamples exist.
main :-
  writeln(
    'Code is verified correct with respect to its specification.').


