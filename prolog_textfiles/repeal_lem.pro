


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/small_lists).

/*
an example that asks whether the sum of outputs of both queries can ever sum to LESS than the size of the source table.
  (and it shows that, presumably surprisingly and helpfully, the answer actually is YES).

remove_nonmatches([X|XT],XR),inverse_remove_nonmatches([X|XT],XR2),length([X|XT],LOT),length(XR,LXR),length(XR2,LXR2),SUM is LXR+LXR2,SUM@<LOT.
  */

person_tuple(
  FIRST,
  MIDDLE,
  LAST) :-

        name_string_type(FIRST), not_null(FIRST),
        name_string_type(MIDDLE),
        name_string_type(LAST), not_null(LAST).



% ----------------------------------------------------------

% the uniqueness contraint (compound key) in this case is not really meant to reflect 'real life'
% it is just a reasonable restriction on the search space in this case.
person_table(L) :-
        % t is the empty mapping, from library assoc
        person_table_with_constraints(L,t,L).


person_table_with_constraints([],_ASSOC,[]).


person_table_with_constraints(
  [(F,M,L)   |LT],
  MAP,
  OUT) :-

        within_table_size_limit(LT),

        person_tuple(F,M,L),

        get_assoc(p(F,M,L),MAP,_EXISTSVAL), % map key (CART_sp) needs to be instantiated by here.

        person_table_with_constraints(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


person_table_with_constraints(
  [(F,M,L)   |LT],
  MAP,
  [(F,M,L)   |REST]) :-

        within_table_size_limit([(F,M,L)   |REST]),

        person_tuple(F,M,L),

        \+get_assoc(p(F,M,L),MAP,_EXISTSVAL),  % map key (p(F,M,L)) needs to be instantiated by here.
        put_assoc(p(F,M,L),MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        person_table_with_constraints(LT,MAP2,REST).



meets_criteria_middle_name(_F,M,_L) :-
        M = jacob.

inverse_criteria(_F,M,_L) :-
        not_null(M),
        M \= jacob.



remove_nonmatches([],[]).
remove_nonmatches([(F,M,L)|X1],[(F,M,L)|Y]) :-
        person_table([(F,M,L)|X1]),
        meets_criteria_middle_name(F,M,L),
        remove_nonmatches(X1,Y).

remove_nonmatches([(F,M,L)|X1],Y) :-
        person_table([(F,M,L)|X1]),
        \+meets_criteria_middle_name(F,M,L),
        remove_nonmatches(X1,Y).



inverse_remove_nonmatches([],[]).
inverse_remove_nonmatches([(F,M,L)|X1],[(F,M,L)|Y]) :-
        person_table([(F,M,L)|X1]),
        inverse_criteria(F,M,L),
        inverse_remove_nonmatches(X1,Y).

inverse_remove_nonmatches([(F,M,L)|X1],Y) :-
        person_table([(F,M,L)|X1]),
        \+inverse_criteria(F,M,L),
        inverse_remove_nonmatches(X1,Y).



run_stuff(_) :-
        remove_nonmatches([X|XT],XR),
        inverse_remove_nonmatches([X|XT],XR2),
        length([X|XT],LOT),
        length(XR,LXR),
        length(XR2,LXR2),
        write('~~~~~~~~~~~~~~~~~~~~~~~~~~~'),nl,
        write([X|XT]),nl,
        write('keep jacobs. size: '), write(LXR),nl,
        write('keep non jacobs. size: '), write(LXR2),nl,
        SUM is LXR+LXR2,
        SUM@<LOT.
