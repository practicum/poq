 %% it_deep.pl   iterative-deepening Prolog meta-interpreter

clause_tree(true,_,_) :- !.
clause_tree(_,D,Limit) :- D > Limit,
                          !,
                          fail.  %% reached depth limit
clause_tree((A,B),D,Limit) :- !,
                              clause_tree(A,D,Limit),
                              clause_tree(B,D,Limit).
clause_tree(A,_,_) :- predicate_property(A,built_in),
                      !,
                      call(A).
clause_tree(A,D,Limit) :- clause(A,B),
                          D1 is D+1,
                          clause_tree(B,D1,Limit).

iterative_deepening(G,L) :- clause_tree(G,0,L).
iterative_deepening(G,L) :- write('limit='),
    nl,
                            write(L),
%                            write('(Hit Enter to Continue.)'),
%                            get0(C),
%                            ( C == 10 ->
                                 L1 is L + 5,
                                 iterative_deepening(G,L1)
%).
.


