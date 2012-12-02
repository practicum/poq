:- module(datatypes,
          [isnull/1,
           nonnull/1,
           demoint/1,
           inttype/1,
           demonat/1,
           nattype/1,
           strtype/1,
           demoname/1,
           fname/1,
           demoword/1,
           wordstr/1,
           demoguid/1,
           guidtype/1,
           tinyint/1,
           barcodeEnum/1]).

% 'barcodeEnum' is clearly specific to the camping database whereas
% inttype and strtype and guidtype are not.  Ideally, we would have
% one module for general-purpose database types and a separate module
% for things like 'barcodeEnum'.  However, in SWIPL this is either not
% possible or else not 'discoverable', because I have not found a way
% to do it GIVEN THE REQUIREMENT that each new type must 'participate'
% in the definition of nonnull.
/*
UPDATE TO THE ABOVE COMMENTS:

  Today I found a predicate called 'multifile' that may solve the
  problem I describe above. See:

  http://www.swi-prolog.org/pldoc/man?predicate=multifile%2f1

  multifile :PredicateIndicator, ...
    Informs the system that the specified predicate(s) may be defined over more than one file. This stops consult/1 from redefining a predicate when a new definition is found.

*/
isnull(null).

% note the use of so-called 'green cut' here.
% (ok... so not 100% green to overlap between inttype and nattype, but this actually is still ok).
% once we SUCCEED AT PROVING that something is nonnull, we no longer care to investigate the other nonnull paths
nonnull(X) :- inttype(X),  !, \+isnull(X).
nonnull(X) :- fname(X),    !, \+isnull(X).
nonnull(X) :- wordstr(X),  !, \+isnull(X).
nonnull(X) :- guidtype(X), !, \+isnull(X).
nonnull(X) :- nattype(X),  !, \+isnull(X).
nonnull(X) :- tinyint(X),  !, \+isnull(X).
nonnull(X) :- barcodeEnum(X),  !, \+isnull(X).

% without the following, other atoms not 'typed' in this file will FAIL a 'nonnull' test.
% for example:  nonnull(abcdefghijk).  will come out FALSE.
nonnull(X) :- \+inttype(X), \+fname(X), \+wordstr(X), \+guidtype(X), \+nattype(X), \+tinyint(X),  \+barcodeEnum(X),
              \+isnull(X).

% after the CUT was added to nonnull, it could no longer be used to GENERATE (retrieve) non-null ground atoms.
% for this reason, please enjoy the use of getnonnull when you need to generate such a thing.
getnonnull(X) :- inttype(X),  \+isnull(X).
getnonnull(X) :- fname(X),    \+isnull(X).
getnonnull(X) :- wordstr(X),  \+isnull(X).
getnonnull(X) :- guidtype(X), \+isnull(X).
getnonnull(X) :- nattype(X),  \+isnull(X).
getnonnull(X) :- tinyint(X),  \+isnull(X).
getnonnull(X) :- barcodeEnum(X),  \+isnull(X).

% without the following, other atoms not 'typed' in this file will FAIL a 'nonnull' test.
% for example:  nonnull(abcdefghijk).  will come out FALSE.
getnonnull(X) :- \+inttype(X), \+fname(X), \+wordstr(X), \+guidtype(X), \+nattype(X), \+tinyint(X),  \+barcodeEnum(X),
              \+isnull(X).

strtype(X) :- wordstr(X).
strtype(X) :- fname(X).

tinyint(null).
tinyint(tinyint_0).
tinyint(tinyint_1).

barcodeEnum(null).
barcodeEnum(full_member_barcode).
barcodeEnum(guest_barcode).

demonat(null).
demonat(0).
demonat(1).
demonat(2).

nattype(X) :- demonat(X).

demoint(X) :- demonat(X).

demoint(-2).
demoint(-1).

inttype(X) :- nattype(X).

demoname(null).
demoname(jacob).
demoname(isabella).
demoname(william).

fname(X) :- demoname(X).


demoword(null).
demoword(dance).
demoword(rural).
demoword(noise).


wordstr(X) :- demoword(X).


demoguid(null).
demoguid(fccy463).
demoguid(srce544).
demoguid(ddd213).
demoguid(tchc397).

guidtype(X) :- demoguid(X).
