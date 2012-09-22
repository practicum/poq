:- module(camping,
          [t_AmenitiesAccessBarcode/4,
           t_Purchase/5,
           t_GuestTrialPeriod/6,
           t_AmenitiesAccessType/2,
           t_barcode_x_purchase/2,
           t_gtperiod_x_purchase/2,
           t_list_type_barcode_x_purchase/1,
           t_list_type_gtperiod_x_purchase/1]).


:- use_module(modules/small_lists).
:- use_module(modules/datatypes).

manageable_list_tail(L) :- size_0_to_1(L). % applied to a TAIL of list, we know the WHOLE list would be +1 bigger

/*
  In structures/functors, using:

  abc - AmenitiesAccessBarcode
  pch - Purchase
  gtp - GuestTrialPeriod
*/

% type definition for an AmenitiesAccessBarcode tuple
t_AmenitiesAccessBarcode(
  BARCODE_STRING,
  BARCODE_TYPE,
  AMENITIES_ID,
  IN_PLAY) :-

        demoguid(BARCODE_STRING),   nonnull(BARCODE_STRING),
        barcodeEnum(BARCODE_TYPE),  nonnull(BARCODE_TYPE),
        demonat(AMENITIES_ID),      nonnull(AMENITIES_ID),
        tinyint(IN_PLAY),           nonnull(IN_PLAY).


% type definition for a Purchase tuple
t_Purchase(
  PURCHASE_ID,
  BARCODE_STRING,
  PURCHASE_DATE,
  PURCHASED_SPACES_QTY,
  CANCELED) :-

        demonat(PURCHASE_ID),          nonnull(PURCHASE_ID),
        demoguid(BARCODE_STRING),      nonnull(BARCODE_STRING),
        demonat(PURCHASE_DATE),        nonnull(PURCHASE_DATE),
        demonat(PURCHASED_SPACES_QTY), nonnull(PURCHASED_SPACES_QTY),
        tinyint(CANCELED),             nonnull(CANCELED).


% type definition for a GuestTrialPeriod tuple
t_GuestTrialPeriod(
  TRIAL_PERIOD_ID,
  BARCODE_STRING,
  GUEST_ID,
  TPERIOD_REDEMPTION_DATE,
  TPERIOD_CONSTRAINT_ID,
  CANCELED) :-

        demonat(TRIAL_PERIOD_ID),         nonnull(TRIAL_PERIOD_ID),
        demoguid(BARCODE_STRING),         nonnull(BARCODE_STRING),
        demonat(GUEST_ID),                nonnull(GUEST_ID),
        demonat(TPERIOD_REDEMPTION_DATE), nonnull(TPERIOD_REDEMPTION_DATE),
        demonat(TPERIOD_CONSTRAINT_ID),   nonnull(TPERIOD_CONSTRAINT_ID),
        tinyint(CANCELED),                nonnull(CANCELED).


% type definition for a AmenitiesAccessType tuple
t_AmenitiesAccessType(
  AMENITIES_ID,
  AMENITIES_ACCESS_LEVEL_NAME) :-

        demonat(AMENITIES_ID),                 nonnull(AMENITIES_ID),
        demoword(AMENITIES_ACCESS_LEVEL_NAME), nonnull(AMENITIES_ACCESS_LEVEL_NAME).


% type definition for a tuple from crossing AmenitiesAccessBarcode(s) with Purchase(s)
t_barcode_x_purchase(
  abc(BARCODE_STRING_abc,
      BARCODE_TYPE,
      AMENITIES_ID,
      IN_PLAY),
  pch(PURCHASE_ID,
      BARCODE_STRING_pch,
      PURCHASE_DATE,
      PURCHASED_SPACES_QTY,
      CANCELED)) :-

        t_AmenitiesAccessBarcode(
            BARCODE_STRING_abc,
            BARCODE_TYPE,
            AMENITIES_ID,
            IN_PLAY),
        t_Purchase(
            PURCHASE_ID,
            BARCODE_STRING_pch,
            PURCHASE_DATE,
            PURCHASED_SPACES_QTY,
            CANCELED).


% type definition for a tuple from crossing GuestTrialPeriod(s) with Purchase(s)
t_gtperiod_x_purchase(
  gtp(TRIAL_PERIOD_ID,
      BARCODE_STRING_gtp,
      GUEST_ID,
      TPERIOD_REDEMPTION_DATE,
      TPERIOD_CONSTRAINT_ID,
      CANCELED_gtp),
  pch(PURCHASE_ID,
      BARCODE_STRING_pch,
      PURCHASE_DATE,
      PURCHASED_SPACES_QTY,
      CANCELED_pch)) :-

        t_GuestTrialPeriod(
            TRIAL_PERIOD_ID,
            BARCODE_STRING_gtp,
            GUEST_ID,
            TPERIOD_REDEMPTION_DATE,
            TPERIOD_CONSTRAINT_ID,
            CANCELED_gtp),
        t_Purchase(
            PURCHASE_ID,
            BARCODE_STRING_pch,
            PURCHASE_DATE,
            PURCHASED_SPACES_QTY,
            CANCELED_pch).


% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
t_list_type_barcode(L) :-
        list_type_abc_removed_dup_barcode(_,L).


list_type_abc_removed_dup_barcode(L,LOUT) :-
        list_type_abc_removed_dup_barcode(L,t,LOUT).


list_type_abc_removed_dup_barcode([],_ASSOC,[]).


list_type_abc_removed_dup_barcode(
  [abc(BARCODE_STRING,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_AmenitiesAccessBarcode(
            BARCODE_STRING,
            BARCODE_TYPE,
            AMENITIES_ID,
            IN_PLAY),
        get_assoc(BARCODE_STRING,MAP,_EXISTSVAL), % map key (BARCODE_STRING) needs to be instantiated by here.

        list_type_abc_removed_dup_barcode(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_abc_removed_dup_barcode(
  [abc(BARCODE_STRING,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |LT],
  MAP,
  [abc(BARCODE_STRING,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |REST]) :-

        manageable_list_tail(LT),
        t_AmenitiesAccessBarcode(
            BARCODE_STRING,
            BARCODE_TYPE,
            AMENITIES_ID,
            IN_PLAY),

        \+get_assoc(BARCODE_STRING,MAP,_EXISTSVAL),  % map key (BARCODE_STRING) needs to be instantiated by here.
        put_assoc(BARCODE_STRING,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_abc_removed_dup_barcode(LT,MAP2,REST).


% ----------------------------------------------------------

% the list to stand in for a 'set' of tuples of type t_barcode_x_purchase
t_list_type_barcode_x_purchase([]).


t_list_type_barcode_x_purchase(
  [abc_x_pch(abc(BARCODE_STRING_abc,
                 BARCODE_TYPE,
                 AMENITIES_ID,
                 IN_PLAY),
             pch(PURCHASE_ID,
                 BARCODE_STRING_pch,
                 PURCHASE_DATE,
                 PURCHASED_SPACES_QTY,
                 CANCELED))   |LT]) :-

        t_barcode_x_purchase(abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
                             pch(PURCHASE_ID,
                                 BARCODE_STRING_pch,
                                 PURCHASE_DATE,
                                 PURCHASED_SPACES_QTY,
                                 CANCELED)),
        manageable_list_tail(LT), % it is very important to put this size PRIOR to the recursion below
        t_list_type_barcode_x_purchase(LT).


% ----------------------------------------------------------

% the list to stand in for a 'set' of tuples of type t_gtperiod_x_purchase
t_list_type_gtperiod_x_purchase([]).


t_list_type_gtperiod_x_purchase(
  [gtp_x_pch(gtp(TRIAL_PERIOD_ID,
                 BARCODE_STRING_gtp,
                 GUEST_ID,
                 TPERIOD_REDEMPTION_DATE,
                 TPERIOD_CONSTRAINT_ID,
                 CANCELED_gtp),
             pch(PURCHASE_ID,
                 BARCODE_STRING_pch,
                 PURCHASE_DATE,
                 PURCHASED_SPACES_QTY,
                 CANCELED_pch))   |LT]) :-

        t_gtperiod_x_purchase(gtp(TRIAL_PERIOD_ID,
                                  BARCODE_STRING_gtp,
                                  GUEST_ID,
                                  TPERIOD_REDEMPTION_DATE,
                                  TPERIOD_CONSTRAINT_ID,
                                  CANCELED_gtp),
                              pch(PURCHASE_ID,
                                  BARCODE_STRING_pch,
                                  PURCHASE_DATE,
                                  PURCHASED_SPACES_QTY,
                                  CANCELED_pch)),
        manageable_list_tail(LT), % it is very important to put this size PRIOR to the recursion below
        t_list_type_gtperiod_x_purchase(LT).


% ----------------------------------------------------------