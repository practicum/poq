:- module(camping_schema,
          [amenitiesAccessBarcodeTuple/4,
           purchaseTuple/5,
           unique_PURCHASE_ID_purchaseTupleList/1]).

:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(datatypes).


amenitiesAccessBarcodeTuple(BARCODE_STRING,BARCODE_TYPE,AMENITIES_ID,IN_PLAY) :-
        demoguid(BARCODE_STRING),nonnull(BARCODE_STRING),
        demoword(BARCODE_TYPE),nonnull(BARCODE_TYPE), % todo: needs own custom enum
        demonat(AMENITIES_ID),nonnull(AMENITIES_ID),
        demonat(IN_PLAY),nonnull(IN_PLAY). % todo: needs boolean/tinyint type

% student_x_scores(student(SID1,NAME),scores(SID2,CID,POINTS)) :-
%         demonat(SID1),
%         demonat(SID2),
%         demoname(NAME),
%         demoguid(CID),
%         demoint(POINTS).

% primary_key_student_sid(L) :-
%         unique_student_sid(L),
%         \+has_null_student_sid(L).

% has_null_student_sid(L) :-
%         member(student(SID,NAME),L),
%         isnull(SID),
%         demoname(NAME).



purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED) :-
        demonat(PURCHASE_ID),nonnull(PURCHASE_ID),
        demoguid(BARCODE_STRING),nonnull(BARCODE_STRING),
        demonat(PURCHASE_DATE),nonnull(PURCHASE_DATE),
        demonat(PURCHASED_SPACES_QTY),nonnull(PURCHASED_SPACES_QTY),
        demonat(CANCELED),nonnull(CANCELED). % todo: needs boolean/tinyint type

unique_PURCHASE_ID_purchaseTupleList([]).

unique_PURCHASE_ID_purchaseTupleList([purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|LT]) :-
         no_dupe_PURCHASE_ID_purchaseTupleList([purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|LT],UX),
         [purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|LT]=UX.

/* t is the empty mapping, from library assoc */
no_dupe_PURCHASE_ID_purchaseTupleList(L,LOUT) :-
        recurse_PURCHASE_ID_purchaseTupleList(L,t,LOUT).

recurse_PURCHASE_ID_purchaseTupleList([],_ASSOC,[]).

recurse_PURCHASE_ID_purchaseTupleList([purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|LT],MAP,OUT) :-
        size_0_to_6(LT), % todo: formalize the rules for this. (perhaps rename to 'limit_size' and provide varying definitions in varying modules)

        demonat(PURCHASE_ID),nonnull(PURCHASE_ID),
        demoguid(BARCODE_STRING),nonnull(BARCODE_STRING),
        demonat(PURCHASE_DATE),nonnull(PURCHASE_DATE),
        demonat(PURCHASED_SPACES_QTY),nonnull(PURCHASED_SPACES_QTY),
        demonat(CANCELED),nonnull(CANCELED),

        get_assoc(PURCHASE_ID,MAP,_EXISTSVAL),
        recurse_PURCHASE_ID_purchaseTupleList(LT,MAP,OUT).

recurse_PURCHASE_ID_purchaseTupleList([purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|LT],
                                      MAP,
                                      [purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|REST]) :-
        size_0_to_6(LT), % todo: formalize the rules for this. (perhaps rename to 'limit_size' and provide varying definitions in varying modules)

        demonat(PURCHASE_ID),nonnull(PURCHASE_ID),
        demoguid(BARCODE_STRING),nonnull(BARCODE_STRING),
        demonat(PURCHASE_DATE),nonnull(PURCHASE_DATE),
        demonat(PURCHASED_SPACES_QTY),nonnull(PURCHASED_SPACES_QTY),
        demonat(CANCELED),nonnull(CANCELED),

        \+get_assoc(PURCHASE_ID,MAP,_EXISTSVAL),
        put_assoc(PURCHASE_ID,MAP,inmap,MAP2),
        recurse_PURCHASE_ID_purchaseTupleList(LT,MAP2,REST).