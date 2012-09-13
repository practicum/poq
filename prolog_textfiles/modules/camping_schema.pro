:- module(camping_schema,
          [amenitiesAccessBarcodeTuple/4,
           purchaseTuple/5,
           unique_PURCHASE_ID_purchaseTupleList/1,
           barcode_x_purchase/2,
           filter_for_join_on_barcode/1,
           filter_on_canceled_zero/1,
           barcode_x_purchase_join_on_barcode/2]).

:- use_module(library(lists)).
:- use_module(library(assoc)).

:- use_module(datatypes).


amenitiesAccessBarcodeTuple(BARCODE_STRING,BARCODE_TYPE,AMENITIES_ID,IN_PLAY) :-
        demoguid(BARCODE_STRING),nonnull(BARCODE_STRING),
        barcodeEnum(BARCODE_TYPE),nonnull(BARCODE_TYPE),
        demonat(AMENITIES_ID),nonnull(AMENITIES_ID),
        tinyint(IN_PLAY),nonnull(IN_PLAY).

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
        tinyint(CANCELED),nonnull(CANCELED).

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

        purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED),

        get_assoc(PURCHASE_ID,MAP,_EXISTSVAL),
        recurse_PURCHASE_ID_purchaseTupleList(LT,MAP,OUT).

recurse_PURCHASE_ID_purchaseTupleList([purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|LT],
                                      MAP,
                                      [purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|REST]) :-
        size_0_to_6(LT), % todo: formalize the rules for this. (perhaps rename to 'limit_size' and provide varying definitions in varying modules)

        purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED),

        \+get_assoc(PURCHASE_ID,MAP,_EXISTSVAL),
        put_assoc(PURCHASE_ID,MAP,inmap,MAP2),
        recurse_PURCHASE_ID_purchaseTupleList(LT,MAP2,REST).

/*
  There seems to be a performance issue somewhere... because the following statement can be shown 'true' at the swipl prompt dozens and dozens of times (if you keep hitting semi-colon to continue)

  Okay... so now we no longer get dozens of dozens of 'true' (dozens of proof trees), but this one still needs some performance-related work:

?- unique_PURCHASE_ID_purchaseTupleList([purchaseTuple(0, fccy463, 0, 0, tinyint_0), purchaseTuple(1, fccy463, 0, 0, tinyint_0), purchaseTuple(2, fccy463, 0, 0, tinyint_1)]).

*/

barcode_x_purchase(amenitiesAccessBarcodeTuple(BARCODE_STRING,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
                   purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)) :-

        amenitiesAccessBarcodeTuple(BARCODE_STRING,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
        purchaseTuple(PURCHASE_ID,BARCODE_STRING,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED).


% TODO: need to handle NULL barcode here! (not for when barcode is a PK, but for the 'general case')
filter_for_join_on_barcode( barcode_x_purchase(amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
                                               purchaseTuple(PURCHASE_ID,BARCODE_STRING2,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)) ) :-

        amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
        purchaseTuple(PURCHASE_ID,BARCODE_STRING2,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED),

        BARCODE_STRING1 = BARCODE_STRING2.

filter_on_canceled_zero( barcode_x_purchase(amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
                                               purchaseTuple(PURCHASE_ID,BARCODE_STRING2,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)) ) :-

        amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
        purchaseTuple(PURCHASE_ID,BARCODE_STRING2,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED),

        CANCELED = tinyint_0.

% NOTE: decision that might need to be examined: if we know we are doing 'join on A=B and C=D' should we make one combined prolog filter procedure or two invididual ones?


barcode_x_purchase_join_on_barcode([],[]).
barcode_x_purchase_join_on_barcode([X0|X1],[X0|Y]) :-
        size_0_to_3(X1),
        filter_for_join_on_barcode(X0),
        barcode_x_purchase_join_on_barcode(X1,Y).

barcode_x_purchase_join_on_barcode([X0|X1],Y) :-
        size_0_to_3(X1),
        \+filter_for_join_on_barcode(X0),
        barcode_x_purchase_join_on_barcode(X1,Y).

cond_canceled_zero([],[]).
cond_canceled_zero([X0|X1],[X0|Y]) :-
        size_0_to_2(X1),
        filter_on_canceled_zero(X0),
        cond_canceled_zero(X1,Y).

cond_canceled_zero([X0|X1],Y) :-
        size_0_to_2(X1),
        \+filter_on_canceled_zero(X0),
        cond_canceled_zero(X1,Y).


crossx_ap([],L2,[]) :-
        size_0_to_12(L2).

crossx_ap(L1,[],[]) :-
        L1 \= [],
        size_0_to_12(L1).

crossx_ap([L1H|L1T],[L2H|L2T],OUT) :-
        size_0_to_12(L1T),
        size_0_to_12(L2T),
        cross_loop_ap(L1H,L1T,[L2H|L2T],[L2H|L2T],OUT).

cross_loop_ap(_SNG,LA,[],LB2,OUT) :-
        crossx_ap(LA,LB2,OUT).

cross_loop_ap(
  amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
  LA,
  [purchaseTuple(PURCHASE_ID,BARCODE_STRING2,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED)|LB1T],
  LB2,
  [barcode_x_purchase(amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
                    purchaseTuple(PURCHASE_ID,BARCODE_STRING2,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED))|O1]) :-

        amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),
        purchaseTuple(PURCHASE_ID,BARCODE_STRING2,PURCHASE_DATE,PURCHASED_SPACES_QTY,CANCELED),

        cross_loop_ap(amenitiesAccessBarcodeTuple(BARCODE_STRING1,BARCODE_TYPE,AMENITIES_ID,IN_PLAY),LA,LB1T,LB2,O1).


myselect_ap(RA,RB,F2) :-
        unique_PURCHASE_ID_purchaseTupleList(RB),
        crossx_ap(RA,RB,RARB),
        barcode_x_purchase_join_on_barcode(RARB,F1),
        cond_canceled_zero(F1,F2).