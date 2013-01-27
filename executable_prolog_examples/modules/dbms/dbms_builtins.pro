:- module(dbms_builtins,

    [agg_field_do_nothing/3,
     agg_field_sum/3,
     agg_field_count/3,
     agg_field_min_atom/3,
     agg_field_max_atom/3,

     agg_base_do_nothing/2,
     agg_base_sum/2,
     agg_base_count/2,
     agg_base_min_atom/2,
     agg_base_max_atom/2,

     agg_empty_do_nothing/1,
     agg_empty_sum/1,
     agg_empty_count/1,
     agg_empty_min_atom/1,
     agg_empty_max_atom/1,

     % this last entry is here so i don't have to move the
     % enclosing ']).' each time i add to the list of exports
     end_of_dbms_builtins_exports_placeholder/0]).


end_of_dbms_builtins_exports_placeholder.

% TODO - need more handling of NULL VALUES.

/*
  whether the winner is set to match PREVIOUS or set to match
  INCOMING *does* affect the query results outcome.  When this is
  important to the proofs of assertions, we should probably
  define BOTH ways.  Further: it may even be desirable to define
  it as capable of producing ANYTHING as the WINNER. that may
  catch more errors.  */
  agg_field_do_nothing(PREVIOUS,_INCOMING,WINNER) :- WINNER =
  PREVIOUS.

agg_field_sum(PREVIOUS,INCOMING,WINNER) :-
        WINNER is PREVIOUS + INCOMING.

agg_field_count(PREVIOUS,_INCOMING,WINNER) :-
        WINNER is PREVIOUS + 1.


% carefully re-read 4.7.1 Standard Order of Terms.
% http://www.swi-prolog.org/pldoc/doc_for?object=section%283,%274.7.1%27,swi%28%27/doc/Manual/compare.html%27%29%29
agg_field_min_atom(PREVIOUS,INCOMING,WINNER) :-
        INCOMING @< PREVIOUS,
        WINNER = INCOMING.

agg_field_min_atom(PREVIOUS,INCOMING,WINNER) :-
        INCOMING @>= PREVIOUS,
        WINNER = PREVIOUS.


agg_field_max_atom(PREVIOUS,INCOMING,WINNER) :-
        INCOMING @> PREVIOUS,
        WINNER = INCOMING.

agg_field_max_atom(PREVIOUS,INCOMING,WINNER) :-
        INCOMING @=< PREVIOUS,
        WINNER = PREVIOUS.

agg_base_do_nothing(INCOMING,OUTPUT) :-
        OUTPUT = INCOMING.

agg_base_sum(INCOMING,OUTPUT) :-
        OUTPUT = INCOMING.

agg_base_count(_INCOMING,1).

agg_base_min_atom(INCOMING,OUTPUT) :-
        OUTPUT = INCOMING.

agg_base_max_atom(INCOMING,OUTPUT) :-
        OUTPUT = INCOMING.

agg_empty_do_nothing(null).

agg_empty_sum(null).

agg_empty_count(0).

agg_empty_min_atom(null).

agg_empty_max_atom(null).

