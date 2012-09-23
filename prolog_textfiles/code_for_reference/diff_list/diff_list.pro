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
  PartListOut-Z) :-
        partsacc(X,
                E-E, % Note: E-E is the empty difference list.
                PartListOut-Z ).

% AccumList. it's like we have some list IN MIND ALREADY. but usually
% if we are not IN THE MIDDLE of building a bike, then what we have in
% mind already so far is NOTHING (aka [], aka E-E).

% to accumulate a *basic* X onto the accum,
% use difference-list concatenation to prepend it.
partsacc(
  X,
  AccumList-Z,
  AccumList-END) :-
        % Note: this clause is the ONLY PLACE where we ALTER the 3rd arg.
        basicpart(X),
        L2-END = [X|END]-END, % make a new list 'L2' consisting of only X and the list's end.
        Z = L2.  % attach L2 where the end of AccumList used to be. (AccumList the SECOND arg).

% notice that X does not make it into the output at all. this is why no 'assembly' names are in the final result
partsacc(
  X,
  AccumList-Z1,
  PartListOut-Z2) :-
        assembly(X, SubParts),  % if X matches some assembly, then...
        % have the traversal procedure add subparts to AccumList
        traverse_expand_partlist(
           SubParts,
           AccumList-Z1,
           PartListOut-Z2).

% nothing else to traverse, so our 'so-far' work is now the output.
traverse_expand_partlist([], AccumList-Z, AccumList-Z).

traverse_expand_partlist(
  [HEAD|TAIL],
  AccumList-Z1,
  PartListOut-Z2) :-
        % have to do mutual recursion and 'start anew' with HEAD. (it could be basic or not)
        partsacc(HEAD,
                 AccumList-Z1,
                 AccumPlusHeadParts-Z3),
        traverse_expand_partlist(TAIL,
                                 AccumPlusHeadParts-Z3,
                                 PartListOut-Z2).

