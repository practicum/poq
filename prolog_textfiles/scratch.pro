:- use_module(library(lists)).
:- use_module(library(assoc)).


:- write('        ~~~~ SCRATCH file opened').

a_descendant(X,Y) :- direct_child(X,Y).
a_descendant(X,Z) :- direct_child(X,Y),a_descendant(Y,Z).

direct_child(granddad,man1).
direct_child(granddad,man2).
direct_child(man2,grandkid).

% offzz(A) :- A.

% $something(X) :- uu(X).  % YAP prolog did not like the $ dollar sign in the predicate name

% mmatch((A,B),B) :- write_canonical(B),nl.

siblings(a1,x1).
siblings(a2,x2).
siblings(a3,x3).

% DEMO of INFINITE tree
siblings(X,Y) :- siblings(Y,X).
% ?- siblings(X,Y) % produces a never-ending stream of assignments

basicpart(rim).
basicpart(spoke).
basicpart(rearframe).
basicpart(handlebars).
basicpart(gears).
basicpart(bolt).
basicpart(nut).
basicpart(fork).

assembly(bike, [wheel,wheel,frame]).
assembly(wheel, [spoke,rim,hub]).
assembly(frame, [rearframe,frontframe]).
assembly(frontframe, [fork,handlebars]).
assembly(hub, [gears,axle]).
assembly(axle, [bolt,nut]).

% if X is basic, we get a list with only X
partsof(X, [X]) :- basicpart(X). % we always end up here eventually. only basic parts are ever in the final list.

% notice that X does not make it into the output at all. this is why no 'assembly' names are in the final result
partsof(X, PartListOut) :-
        assembly(X, PartList2), % X matches some assembly
        traverse_expand_partlist(PartList2, PartListOut). % traversing components of X generates PartListOut

traverse_expand_partlist([],[]). % traversing nothing gets you nothing back.

traverse_expand_partlist([HEAD|TAIL], PartListOut) :-
        partsof(HEAD, HeadParts), % have to do mutual recursion and 'start anew' with HEAD. (it could be basic or not)
        traverse_expand_partlist(TAIL, TailParts), % traverse the remaining component list
        append(HeadParts, TailParts, PartListOut).


% just a 'wrapper' to give us a 2-arg version
partsof2(X, PartListOut) :- partsacc(X, [], PartListOut).

% AccumList. it's like we have some list IN MIND ALREADY. but usually if we are not IN THE MIDDLE of building a bike, then
% what we have in mind already so far is NOTHING (aka []).

% to accumulate a *basic* X onto the accum, just add it in front
partsacc(X, AccumList, [X|AccumList]) :- basicpart(X). % this is the ONLY PLACE where we ALTER the 3rd arg.

% notice that X does not make it into the output at all. this is why no 'assembly' names are in the final result
partsacc(X, AccumList, PartListOut) :-
        assembly(X, SubParts), % if X matches some assembly, then...
        traverse_expand_partlist2(SubParts, AccumList, PartListOut). % have the traversal procedure add subparts to AccumList

traverse_expand_partlist2([], AccumList, AccumList). % nothing else to traverse, so our 'so-far' work is now the output.

traverse_expand_partlist2([HEAD|TAIL], AccumList, PartListOut) :-
        % have to do mutual recursion and 'start anew' with HEAD. (it could be basic or not)
        partsacc(HEAD, AccumList, AccumPlusHeadParts),
        traverse_expand_partlist2(TAIL, AccumPlusHeadParts, PartListOut).
        % now there is no 'append' line here. good job.




