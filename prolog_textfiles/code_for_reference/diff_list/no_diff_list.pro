:- use_module(library(lists)).
:- use_module(library(assoc)).


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

% just a 'wrapper' to give us a 2-arg version
partsof(
  X,
  PartListOut) :-
        partsacc(X,
                 [],
                 PartListOut ).

% AccumList. it's like we have some list IN MIND ALREADY. but usually
% if we are not IN THE MIDDLE of building a bike, then what we have in
% mind already so far is NOTHING (aka [], aka E-E).

% to accumulate a *basic* X onto the accum,
% just add it in front
partsacc(
  X,
  AccumList,
  [X|AccumList]) :-
        % Note: this clause is the ONLY PLACE where we ALTER the 3rd arg.
        basicpart(X).

% notice that X does not make it into the output at all. this is why no 'assembly' names are in the final result
partsacc(
  X,
  AccumList,
  PartListOut) :-
        assembly(X, SubParts),  % if X matches some assembly, then...
        % have the traversal procedure add subparts to AccumList
        traverse_expand_partlist(
           SubParts,
           AccumList,
           PartListOut).

% nothing else to traverse, so our 'so-far' work is now the output.
traverse_expand_partlist([], AccumList, AccumList).

traverse_expand_partlist(
  [HEAD|TAIL],
  AccumList,
  PartListOut) :-
        % have to do mutual recursion and 'start anew' with HEAD. (it could be basic or not)
        partsacc(HEAD,
                 AccumList,
                 AccumPlusHeadParts),
        traverse_expand_partlist(TAIL,
                                 AccumPlusHeadParts,
                                 PartListOut).

