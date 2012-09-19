:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(modules/small_lists).
:- use_module(modules/datatypes).

:- write('        ~~~~ SCRATCH file opened').

a_descendant(X,Y) :- direct_child(X,Y).
a_descendant(X,Z) :- direct_child(X,Y),a_descendant(Y,Z).

direct_child(granddad,man1).
direct_child(granddad,man2).
direct_child(man2,grandkid).

% offzz(A) :- A.

say_hi :- write('saying hi').

uu(one) :- say_hi.
uu(two) :- say_hi.
uu(three) :- say_hi.

vv(_) :- demonat(_), uu(_).

% $something(X) :- uu(X).  % YAP prolog did not like the $ dollar sign in the predicate name

/*
  Exit: (10) clause
  (  (direct_child(man1, _G443), a_descendant(_G443, grandkid)),

  (call(direct_child(man1, _G443)), call(a_descendant(_G443, grandkid)))) ?

  */

% without the CUT for matching 'true', upon backtracking we take 'true' and match it to the FINAL mini_solve predicate,
% which causes us to then call clause(true,B), which generates:
%    ERROR: clause/2: No permission to access private_procedure `true/0'
mini_solve(true) :- !.

% apparently (by observation, but i have not found documentation yet), after we already have done unification
% of everything in the explicit program during the 'clause(A,B)' execution, we ultimately unify with:
%   clause(ANYTHING,call(ANYTHING)) ... and then without this version of mini_solve we loop endlessly on that.
mini_solve(call(_)) :- !, fail.

mini_solve((A,B)) :- mini_solve(A), mini_solve(B).
mini_solve(A) :- clause(A,B), mini_solve(B).

% mmatch((A,B),B) :- write_canonical(B),nl.