:- module(datatypes,
          [isnull/1,
           nonnull/1,
           int_type/1,
           natural_type/1,
           name_type/1,
           word_type/1,
           guid_type/1,
           tinyint/1,
           barcodeEnum/1]).

% 'barcodeEnum' is clearly specific to the camping database whereas
% int_type and and guid_type are not.  Ideally, we would have
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
% (ok... so not 100% green to overlap between int_type and natural_type, but this actually is still ok).
% once we SUCCEED AT PROVING that something is nonnull, we no longer care to investigate the other nonnull paths
nonnull(X) :- int_type(X),  !, \+isnull(X).
nonnull(X) :- name_type(X),    !, \+isnull(X).
nonnull(X) :- word_type(X),  !, \+isnull(X).
nonnull(X) :- guid_type(X), !, \+isnull(X).
nonnull(X) :- natural_type(X),  !, \+isnull(X).
nonnull(X) :- tinyint(X),  !, \+isnull(X).
nonnull(X) :- barcodeEnum(X),  !, \+isnull(X).

% without the following, other atoms not 'typed' in this file will FAIL a 'nonnull' test.
% for example:  nonnull(abcdefghijk).  will come out FALSE.
nonnull(X) :- \+int_type(X), \+name_type(X), \+word_type(X), \+guid_type(X), \+natural_type(X), \+tinyint(X),  \+barcodeEnum(X),
              \+isnull(X).

% after the CUT was added to nonnull, it could no longer be used to GENERATE (retrieve) non-null ground atoms.
% for this reason, please enjoy the use of getnonnull when you need to generate such a thing.
getnonnull(X) :- int_type(X),  \+isnull(X).
getnonnull(X) :- name_type(X),    \+isnull(X).
getnonnull(X) :- word_type(X),  \+isnull(X).
getnonnull(X) :- guid_type(X), \+isnull(X).
getnonnull(X) :- natural_type(X),  \+isnull(X).
getnonnull(X) :- tinyint(X),  \+isnull(X).
getnonnull(X) :- barcodeEnum(X),  \+isnull(X).

% without the following, other atoms not 'typed' in this file will FAIL a 'nonnull' test.
% for example:  nonnull(abcdefghijk).  will come out FALSE.
getnonnull(X) :- \+int_type(X), \+name_type(X), \+word_type(X), \+guid_type(X), \+natural_type(X), \+tinyint(X),  \+barcodeEnum(X),
              \+isnull(X).



tinyint(null).
tinyint(tinyint_0).
tinyint(tinyint_1).

barcodeEnum(null).
barcodeEnum(full_member_barcode).
barcodeEnum(guest_barcode).

natural_type(null).
natural_type(0).
natural_type(1).
natural_type(2).
natural_type(3).
natural_type(4).

int_type(X) :- natural_type(X).

int_type(-2).
int_type(-1).


name_type(null).
name_type(jacob).
name_type(isabella).
name_type(william).



word_type(null).
word_type(dance).
word_type(rural).
word_type(noise).



guid_type(null).
guid_type(fccy463).
guid_type(srce544).
guid_type(ddd213).
guid_type(tchc397).


