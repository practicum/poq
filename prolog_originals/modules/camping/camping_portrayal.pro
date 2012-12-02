:- module(camping_portrayal,
          [print_abc_table/1]).


/*
customizable printing of terms:

portray(+Term)
    A dynamic predicate, which can be defined by the user to change the behaviour of print/1 on (sub)terms. For each subterm encountered that is not a variable print/1 first calls portray/1 using the term as argument. For lists, only the list as a whole is given to portray/1. If portray/1 succeeds print/1 assumes the term has been written.
*/

print_abc_table_row(ROW) :-
        write(ROW),
        nl.

print_abc_table([H|[]]) :-
        !,
        write(H).

print_abc_table([H|T]) :-
        print_abc_table_row(H),
        print_abc_table(T).


user:portray(L) :-
        t_list_type_barcode(L),
        write('AmenitiesAccessBarcode table:'),
        nl,
        print_abc_table(L).
