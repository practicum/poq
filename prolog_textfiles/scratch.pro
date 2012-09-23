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
partsof2(X, PartListOut) :- partsacc2(X, [], PartListOut).

% AccumList. it's like we have some list IN MIND ALREADY. but usually if we are not IN THE MIDDLE of building a bike, then
% what we have in mind already so far is NOTHING (aka []).

% to accumulate a *basic* X onto the accum, just add it in front
partsacc2(X, AccumList, [X|AccumList]) :- basicpart(X). % this is the ONLY PLACE where we ALTER the 3rd arg.

% notice that X does not make it into the output at all. this is why no 'assembly' names are in the final result
partsacc2(X, AccumList, PartListOut) :-
        assembly(X, SubParts), % if X matches some assembly, then...
        traverse_expand_partlist2(SubParts, AccumList, PartListOut). % have the traversal procedure add subparts to AccumList

traverse_expand_partlist2([], AccumList, AccumList). % nothing else to traverse, so our 'so-far' work is now the output.

traverse_expand_partlist2([HEAD|TAIL], AccumList, PartListOut) :-
        % have to do mutual recursion and 'start anew' with HEAD. (it could be basic or not)
        partsacc2(HEAD, AccumList, AccumPlusHeadParts),
        traverse_expand_partlist2(TAIL, AccumPlusHeadParts, PartListOut).
        % now there is no 'append' line here. good job.

demo_incomplete_struct(OUT) :-
        OUT = [a,b,c|X],
        patch_with_THING(X,THING), % X will 'share' with prior line.
        THING = [z,y,x].               % THING will 'share' with prior line.

patch_with_THING(T,T).


demo_incomplete_struct2(OUT) :-
        OUT = [a,b,c|X],
        patch_with_THING2(X,THING), % X will 'share' with prior line.
        THING = [].               % THING will 'share' with prior line.

patch_with_THING2([huh|T],T).


demo_incomplete_struct3(OUT) :-
        OUT = [a,b,c|Hole1],
        patch_with_THING3(Hole1,_Hole2). % X will 'share' with prior line.


patch_with_THING3([huh,wow,wut|Hole],Hole).


demo_incomplete_struct4(OUT) :-
        OUT = [a,b,c|Hole1],          % as long as we LEAVE A PLACEHOLDER
        Hole1 = [huh,wow,wut|_Hole2]. % then we can instantiate the placeholder to whatever, and 'Hole1' is *sharing* with prior line.


/*
now we make a difference list that looks like:
  DATA_LIST-ENDNAME

if both are the same, then essentially we assume we have:
  ENDNAME-ENDNAME
    ... which of course would be an empty list, because all it contains is its own end
*/

sample_concat( A1-Z1, Z1-Z2, A1-Z2 ).

/*
?- sample_concat( [a,b,c|T1]-T1,[d,e|T2]-T2,L).
T1 = [d, e|T2],
L = [a, b, c, d, e|T2]-T2.
*/


% just a 'wrapper' to give us a 2-arg version
partsof3(X, PartListOut-Z) :- partsacc3(X, E-E, PartListOut-Z). % Note: E-E is the empty difference list.

% AccumList-Z. it's like we have some list IN MIND ALREADY. but usually if we are not IN THE MIDDLE of building a bike, then
% what we have in mind already so far is NOTHING (aka [], aka E-E).

% to accumulate a *basic* X onto the accum, use difference-list concatenation to prepend it.
partsacc3(X, AccumList-Z, AccumList-END) :- % this is the ONLY PLACE where we ALTER the 3rd arg.
        basicpart(X),
        L2-END = [X|END]-END, % make a new list 'L2' consisting of only X and the list's end.
        Z = L2.  % attach L2 where the end of AccumList used to be. (AccumList the SECOND arg).

% notice that X does not make it into the output at all. this is why no 'assembly' names are in the final result
partsacc3(X, AccumList-Z1, PartListOut-Z2) :-
        assembly(X, SubParts), % if X matches some assembly, then...
        % have the traversal procedure add subparts to AccumList-Z
        traverse_expand_partlist3(SubParts, AccumList-Z1, PartListOut-Z2).

traverse_expand_partlist3([], AccumList-Z, AccumList-Z). % nothing else to traverse, so our 'so-far' work is now the output.

traverse_expand_partlist3([HEAD|TAIL], AccumList-Z1, PartListOut-Z2) :-
        % have to do mutual recursion and 'start anew' with HEAD. (it could be basic or not)
        partsacc3(HEAD, AccumList-Z1, AccumPlusHeadParts-Z3),
        traverse_expand_partlist3(TAIL, AccumPlusHeadParts-Z3, PartListOut-Z2).
        % now there is no 'append' line here. good job.
