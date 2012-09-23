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
        Z = [X|END].  % attach L2 where the end of AccumList used to be. (AccumList the SECOND arg).

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


% just a 'wrapper' to give us a PURE LIST (not diff-list) result
parts_clean(X,L) :-
        partsof(X,L-[]).



% Dijkstra's Dutch flag (simplified version?) as shown in The Art Of Prolog

solve_dutch_flag(
  INPUT_LIST,
  FINAL_OUT) :-
        dflag_distribute(INPUT_LIST,
                         FINAL_OUT-RZ,
                         W-WZ,
                         B-[]),  % make [] be the END of blues
        WZ=B, % make blues be the END of whites
        RZ=W. % make whites be the END of reds


dflag_distribute(
  [],
  RZ-RZ,
  WZ-WZ,
  BZ-BZ).

% head is red
dflag_distribute(
  [red(X)|TAIL],
  [red(X)|R]-RZ,
  W,
  B) :-
        dflag_distribute(
                         TAIL,
                         R-RZ,
                         W,
                         B).

% head is white
dflag_distribute(
  [white(X)|TAIL],
  R,
  [white(X)|W]-WZ,
  B) :-
        dflag_distribute(
                         TAIL,
                         R,
                         W-WZ,
                         B).

% head is blue
dflag_distribute(
  [blue(X)|TAIL],
  R,
  W,
  [blue(X)|B]-BZ) :-
        dflag_distribute(
                         TAIL,
                         R,
                         W,
                         B-BZ).

/*
?- dflag_distribute([red(1),white(2),blue(3),red(4),white(5)],R,W,B).
R = [red(1), red(4)|_G48]-_G48,
W = [white(2), white(5)|_G59]-_G59,
B = [blue(3)|_G70]-_G70 ;
false.

?- dflag_distribute([red(1),white(2),blue(3),red(4),white(5)],R-RZ,W-WZ,B-BZ).
R = [red(1), red(4)|RZ],
W = [white(2), white(5)|WZ],
B = [blue(3)|BZ] ;
false.

?- dflag_distribute([red(1),white(2),blue(3),red(4),white(5)],R-RZ,W-WZ,B-BZ),WZ=B.
R = [red(1), red(4)|RZ],
W = [white(2), white(5), blue(3)|BZ],
WZ = B, B = [blue(3)|BZ] ;
false.

?- dflag_distribute([red(1),white(2),blue(3),red(4),white(5)],R-RZ,W-WZ,B-BZ),WZ=B,RZ=W.
R = [red(1), red(4), white(2), white(5), blue(3)|BZ],
RZ = W, W = [white(2), white(5), blue(3)|BZ],
WZ = B, B = [blue(3)|BZ] ;
false.



*/

as_in_book(INPUT_LIST,R) :-
        dflag_distribute(INPUT_LIST, R-W, W-B, B-[]).
%        WZ=B,
%        RZ=W.
