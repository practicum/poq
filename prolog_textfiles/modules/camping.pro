:- module(camping,
          [t_AmenitiesAccessBarcode/4,
           t_Purchase/5,
           t_GuestTrialPeriod/6,
           t_AmenitiesAccessType/2,
           t_barcode_x_purchase/2,
           t_gtperiod_x_purchase/2,
           t_list_type_barcode/1,  % actually wraps list_type_abc_removed_dup_barcode/3
           t_list_type_purchase/1,  % actually wraps list_type_pch_removed_dup_pchid/3
           t_list_type_gtperiod/1, % actually wraps list_type_gtp_removed_dup_gtpid/3

           t_list_type_barcode_x_purchase/1,
           t_list_type_gtperiod_x_purchase/1,
           cross_barcode_purchase/3,
           cross_barcode_gtperiod/3]).


:- use_module(modules/small_lists).
%:- use_module(modules/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

manageable_list_tail(L) :- size_0_to_1(L). % applied to a TAIL of list, we know the WHOLE list would be +1 bigger
%manageable_list_tail(L) :- length(L,1);length(L,0). % sometimes it is helpful to reverse the order of the permissible lengths

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
        % t is the empty mapping, from library assoc
        list_type_abc_removed_dup_barcode(L,t,L).


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

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
t_list_type_purchase(L) :-
        % t is the empty mapping, from library assoc
        list_type_pch_removed_dup_pchid(L,t,L).


list_type_pch_removed_dup_pchid([],_ASSOC,[]).


list_type_pch_removed_dup_pchid(
  [pch(UNIQUE_FIELD, % PURCHASE_ID
       BARCODE_STRING,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_Purchase(UNIQUE_FIELD,       % PURCHASE_ID
                   BARCODE_STRING,
                   PURCHASE_DATE,
                   PURCHASED_SPACES_QTY,
                   CANCELED),
        get_assoc(UNIQUE_FIELD,MAP,_EXISTSVAL), % map key (UNIQUE_FIELD) needs to be instantiated by here.

        list_type_pch_removed_dup_pchid(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_pch_removed_dup_pchid(
  [pch(UNIQUE_FIELD, % PURCHASE_ID
       BARCODE_STRING,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |LT],
  MAP,
  [pch(UNIQUE_FIELD, % PURCHASE_ID
       BARCODE_STRING,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |REST]) :-

        manageable_list_tail(LT),
        t_Purchase(UNIQUE_FIELD,       % PURCHASE_ID
                   BARCODE_STRING,
                   PURCHASE_DATE,
                   PURCHASED_SPACES_QTY,
                   CANCELED),

        \+get_assoc(UNIQUE_FIELD,MAP,_EXISTSVAL),  % map key (UNIQUE_FIELD) needs to be instantiated by here.
        put_assoc(UNIQUE_FIELD,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_pch_removed_dup_pchid(LT,MAP2,REST).


% ----------------------------------------------------------

% putting the UNIQUE information here.  TODO: what if two columns bore the unique keyword?
t_list_type_gtperiod(L) :-
        % t is the empty mapping, from library assoc
        list_type_gtp_removed_dup_gtpid(L,t,L).


list_type_gtp_removed_dup_gtpid([],_ASSOC,[]).


list_type_gtp_removed_dup_gtpid(
  [gtp(UNIQUE_FIELD,              %TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)   |LT],
  MAP,
  OUT) :-

        manageable_list_tail(LT),
        t_GuestTrialPeriod(UNIQUE_FIELD,          %TRIAL_PERIOD_ID,
                           BARCODE_STRING_gtp,
                           GUEST_ID,
                           TPERIOD_REDEMPTION_DATE,
                           TPERIOD_CONSTRAINT_ID,
                           CANCELED_gtp),
        get_assoc(UNIQUE_FIELD,MAP,_EXISTSVAL), % map key (UNIQUE_FIELD) needs to be instantiated by here.

        list_type_gtp_removed_dup_gtpid(LT,MAP,OUT). % note: here, the OUT (output) does NOT include the head item.


list_type_gtp_removed_dup_gtpid(
  [gtp(UNIQUE_FIELD,              %TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)   |LT],
  MAP,
  [gtp(UNIQUE_FIELD,              %TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)   |REST]) :-

        manageable_list_tail(LT),
        t_GuestTrialPeriod(UNIQUE_FIELD,          %TRIAL_PERIOD_ID,
                           BARCODE_STRING_gtp,
                           GUEST_ID,
                           TPERIOD_REDEMPTION_DATE,
                           TPERIOD_CONSTRAINT_ID,
                           CANCELED_gtp),

        \+get_assoc(UNIQUE_FIELD,MAP,_EXISTSVAL),  % map key (UNIQUE_FIELD) needs to be instantiated by here.
        put_assoc(UNIQUE_FIELD,MAP,inmap,MAP2),    % 'inmap' is an arbitrary ground value to link with the key.
        list_type_gtp_removed_dup_gtpid(LT,MAP2,REST).


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

/*
There are 7 different clauses to express cross_barcode_purchase.

There should be no duplication in outcomes due to careful management
of when each of the 7 clauses is allowed to be applied.

Each one of the 7 handles a NON-OVERLAPPING subset of cases based on
the SIZE of the first two list variables.

The cases (by size of the two lists) are:

[]    []
1     []
[]    1
1     >1
1+    1     (1+ means 'one or more')
2+    2+  ... and the first list size is greater to or EQUAL to the second
2+    2+  ... and the first list size is LESS THAN the second
*/
% IMPORTANT. IMPORTANT: roll back to commit 0ebccc69c58c1c6 to see a 'pure crossing' version with no join conditions
cross_barcode_purchase( [], [], [] ).


cross_barcode_purchase(
  [abc(BARCODE_STRING,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |[]],
  [],
  [] ) :-

        t_AmenitiesAccessBarcode(
            BARCODE_STRING,
            BARCODE_TYPE,
            AMENITIES_ID,
            IN_PLAY).


cross_barcode_purchase(
  [],
  [pch(PURCHASE_ID,
       BARCODE_STRING,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |[]],
  [] ) :-

        t_Purchase(
            PURCHASE_ID,
            BARCODE_STRING,
            PURCHASE_DATE,
            PURCHASED_SPACES_QTY,
            CANCELED).


% single barcode but longer list of purchase, MEETS JOIN conditions
cross_barcode_purchase(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |[]],
  [pch(PURCHASE_ID,
       BARCODE_STRING_pch,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |L2T],
  [abc_pch(abc(BARCODE_STRING_abc,
               BARCODE_TYPE,
               AMENITIES_ID,
               IN_PLAY),
           pch(PURCHASE_ID,
               BARCODE_STRING_pch,
               PURCHASE_DATE,
               PURCHASED_SPACES_QTY,
               CANCELED))   |R]  ) :-

        t_AmenitiesAccessBarcode(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        t_list_type_purchase([pch(PURCHASE_ID,
                                  BARCODE_STRING_pch,
                                  PURCHASE_DATE,
                                  PURCHASED_SPACES_QTY,
                                  CANCELED)   |L2T]),
        length([pch(PURCHASE_ID,
                    BARCODE_STRING_pch,
                    PURCHASE_DATE,
                    PURCHASED_SPACES_QTY,
                    CANCELED)   |L2T],X),
        X>1,
        manageable_list_tail(L2T),
        BARCODE_STRING_abc = BARCODE_STRING_pch, % join condition
        cross_barcode_purchase( [abc(BARCODE_STRING_abc,
                                     BARCODE_TYPE,
                                     AMENITIES_ID,
                                     IN_PLAY)   |[]], L2T, R ).


% single barcode but longer list of purchase, FAILS TO MEET JOIN conditions
cross_barcode_purchase(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |[]],
  [pch(PURCHASE_ID,
       BARCODE_STRING_pch,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |L2T],
  R ) :-

        t_AmenitiesAccessBarcode(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        t_list_type_purchase([pch(PURCHASE_ID,
                                  BARCODE_STRING_pch,
                                  PURCHASE_DATE,
                                  PURCHASED_SPACES_QTY,
                                  CANCELED)   |L2T]),
        length([pch(PURCHASE_ID,
                    BARCODE_STRING_pch,
                    PURCHASE_DATE,
                    PURCHASED_SPACES_QTY,
                    CANCELED)   |L2T],X),
        X>1,
        manageable_list_tail(L2T),
        BARCODE_STRING_abc \= BARCODE_STRING_pch, % negation/complement of join condition
        cross_barcode_purchase( [abc(BARCODE_STRING_abc,
                                     BARCODE_TYPE,
                                     AMENITIES_ID,
                                     IN_PLAY)   |[]], L2T, R ).


% longer barcode list but SINGLE purchase, MEETS JOIN conditions
cross_barcode_purchase(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |L2T],
  [pch(PURCHASE_ID,
       BARCODE_STRING_pch,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |[]],
  [abc_pch(abc(BARCODE_STRING_abc,
               BARCODE_TYPE,
               AMENITIES_ID,
               IN_PLAY),
           pch(PURCHASE_ID,
               BARCODE_STRING_pch,
               PURCHASE_DATE,
               PURCHASED_SPACES_QTY,
               CANCELED))   |R]  ) :-

        t_list_type_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L2T]),
        t_Purchase(PURCHASE_ID,
                   BARCODE_STRING_pch,
                   PURCHASE_DATE,
                   PURCHASED_SPACES_QTY,
                   CANCELED),
        manageable_list_tail(L2T),
        BARCODE_STRING_abc = BARCODE_STRING_pch, % join condition
        cross_barcode_purchase( L2T,
                                [pch(PURCHASE_ID,
                                     BARCODE_STRING_pch,
                                     PURCHASE_DATE,
                                     PURCHASED_SPACES_QTY,
                                     CANCELED)   |[]],
                                R ).


% longer barcode list but SINGLE purchase, FAILS TO MEET JOIN conditions
cross_barcode_purchase(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |L2T],
  [pch(PURCHASE_ID,
       BARCODE_STRING_pch,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |[]],
  R ) :-

        t_list_type_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L2T]),
        t_Purchase(PURCHASE_ID,
                   BARCODE_STRING_pch,
                   PURCHASE_DATE,
                   PURCHASED_SPACES_QTY,
                   CANCELED),
        manageable_list_tail(L2T),
        BARCODE_STRING_abc \= BARCODE_STRING_pch, % negation/complement of join condition
        cross_barcode_purchase( L2T,
                                [pch(PURCHASE_ID,
                                     BARCODE_STRING_pch,
                                     PURCHASE_DATE,
                                     PURCHASED_SPACES_QTY,
                                     CANCELED)   |[]],
                                R ).


% adding one more purchase to an 'already crossing'
cross_barcode_purchase(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |L1T], % this list needs to be nonempty. the empty case is handled elsewhere
  [pch(PURCHASE_ID,
       BARCODE_STRING_pch,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)   |L2T],
  FINAL ) :-

        t_list_type_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L1T]),
        t_list_type_purchase([pch(PURCHASE_ID,
                                  BARCODE_STRING_pch,
                                  PURCHASE_DATE,
                                  PURCHASED_SPACES_QTY,
                                  CANCELED)   |L2T]),
        length([abc(BARCODE_STRING_abc,
                    BARCODE_TYPE,
                    AMENITIES_ID,
                    IN_PLAY)   |L1T],
               X),
        X>1,
        length([pch(PURCHASE_ID,
                    BARCODE_STRING_pch,
                    PURCHASE_DATE,
                    PURCHASED_SPACES_QTY,
                    CANCELED)   |L2T],
               Y),
        Y>1,
        X>=Y,
        cross_barcode_purchase([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |L1T],
                               L2T,
                               POUT),
        cross_barcode_purchase([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |L1T],
                               [pch(PURCHASE_ID,
                                    BARCODE_STRING_pch,
                                    PURCHASE_DATE,
                                    PURCHASED_SPACES_QTY,
                                    CANCELED)   |[]],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more barcode to an 'already crossing'
cross_barcode_purchase(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |L1T],
  [pch(PURCHASE_ID,
       BARCODE_STRING_pch,
       PURCHASE_DATE,
       PURCHASED_SPACES_QTY,
       CANCELED)  |D], % this list needs to be nonempty. the empty case is handled elsewhere
  FINAL ) :-

        t_list_type_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L1T]),
        t_list_type_purchase([pch(PURCHASE_ID,
                                  BARCODE_STRING_pch,
                                  PURCHASE_DATE,
                                  PURCHASED_SPACES_QTY,
                                  CANCELED)   |D]),
        length([abc(BARCODE_STRING_abc,
                    BARCODE_TYPE,
                    AMENITIES_ID,
                    IN_PLAY)   |L1T],
               X),
        X>1,
        length([pch(PURCHASE_ID,
                    BARCODE_STRING_pch,
                    PURCHASE_DATE,
                    PURCHASED_SPACES_QTY,
                    CANCELED)   |D],
               Y),
        Y>1,
        X<Y,
        cross_barcode_purchase(L1T,
                               [pch(PURCHASE_ID,
                                    BARCODE_STRING_pch,
                                    PURCHASE_DATE,
                                    PURCHASED_SPACES_QTY,
                                    CANCELED)   |D],
                               POUT),
        cross_barcode_purchase([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |[]],
                               [pch(PURCHASE_ID,
                                    BARCODE_STRING_pch,
                                    PURCHASE_DATE,
                                    PURCHASED_SPACES_QTY,
                                    CANCELED)   |D],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% ----------------------------------------------------------

/*
There are 7 different clauses for the next predicate.
Refer to the notes above (for cross_barcode_purchase) for more details.
*/
cross_barcode_gtperiod( [], [], [] ).


cross_barcode_gtperiod(
  [abc(BARCODE_STRING,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |[]],
  [],
  [] ) :-

        t_AmenitiesAccessBarcode(
            BARCODE_STRING,
            BARCODE_TYPE,
            AMENITIES_ID,
            IN_PLAY).


cross_barcode_gtperiod(
  [],
  [gtp(TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)   |[]],
  [] ) :-

        t_GuestTrialPeriod(
            TRIAL_PERIOD_ID,
            BARCODE_STRING_gtp,
            GUEST_ID,
            TPERIOD_REDEMPTION_DATE,
            TPERIOD_CONSTRAINT_ID,
            CANCELED_gtp).


% single barcode but longer list of gtperiod(s)
cross_barcode_gtperiod(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |[]],
  [gtp(TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)   |L2T],
  [abc_gtp(abc(BARCODE_STRING_abc,
               BARCODE_TYPE,
               AMENITIES_ID,
               IN_PLAY),
           gtp(TRIAL_PERIOD_ID,
               BARCODE_STRING_gtp,
               GUEST_ID,
               TPERIOD_REDEMPTION_DATE,
               TPERIOD_CONSTRAINT_ID,
               CANCELED_gtp))   |R]  ) :-

        t_AmenitiesAccessBarcode(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        t_list_type_gtperiod([gtp(TRIAL_PERIOD_ID,
                                  BARCODE_STRING_gtp,
                                  GUEST_ID,
                                  TPERIOD_REDEMPTION_DATE,
                                  TPERIOD_CONSTRAINT_ID,
                                  CANCELED_gtp)   |L2T]),
        length([gtp(TRIAL_PERIOD_ID,
                    BARCODE_STRING_gtp,
                    GUEST_ID,
                    TPERIOD_REDEMPTION_DATE,
                    TPERIOD_CONSTRAINT_ID,
                    CANCELED_gtp)   |L2T],X),
        X>1,
        manageable_list_tail(L2T),
        cross_barcode_gtperiod( [abc(BARCODE_STRING_abc,
                                     BARCODE_TYPE,
                                     AMENITIES_ID,
                                     IN_PLAY)   |[]], L2T, R ).

% longer barcode list but SINGLE gtperiod
cross_barcode_gtperiod(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |L2T],
  [gtp(TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)   |[]],
  [abc_gtp(abc(BARCODE_STRING_abc,
               BARCODE_TYPE,
               AMENITIES_ID,
               IN_PLAY),
           gtp(TRIAL_PERIOD_ID,
               BARCODE_STRING_gtp,
               GUEST_ID,
               TPERIOD_REDEMPTION_DATE,
               TPERIOD_CONSTRAINT_ID,
               CANCELED_gtp))   |R]  ) :-

        t_list_type_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L2T]),
        t_GuestTrialPeriod(TRIAL_PERIOD_ID,
                           BARCODE_STRING_gtp,
                           GUEST_ID,
                           TPERIOD_REDEMPTION_DATE,
                           TPERIOD_CONSTRAINT_ID,
                           CANCELED_gtp),
        manageable_list_tail(L2T),
        cross_barcode_gtperiod( L2T,
                                [gtp(TRIAL_PERIOD_ID,
                                     BARCODE_STRING_gtp,
                                     GUEST_ID,
                                     TPERIOD_REDEMPTION_DATE,
                                     TPERIOD_CONSTRAINT_ID,
                                     CANCELED_gtp)   |[]],
                                R ).


% adding one more gtperiod to an 'already crossing'
cross_barcode_gtperiod(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |L1T], % this list needs to be nonempty. the empty case is handled elsewhere
  [gtp(TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)   |L2T],
  FINAL ) :-

        t_list_type_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L1T]),
        t_list_type_gtperiod([gtp(TRIAL_PERIOD_ID,
                                  BARCODE_STRING_gtp,
                                  GUEST_ID,
                                  TPERIOD_REDEMPTION_DATE,
                                  TPERIOD_CONSTRAINT_ID,
                                  CANCELED_gtp)   |L2T]),
        length([abc(BARCODE_STRING_abc,
                    BARCODE_TYPE,
                    AMENITIES_ID,
                    IN_PLAY)   |L1T],
               X),
        X>1,
        length([gtp(TRIAL_PERIOD_ID,
                    BARCODE_STRING_gtp,
                    GUEST_ID,
                    TPERIOD_REDEMPTION_DATE,
                    TPERIOD_CONSTRAINT_ID,
                    CANCELED_gtp)   |L2T],
               Y),
        Y>1,
        X>=Y,
        cross_barcode_gtperiod([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |L1T],
                               L2T,
                               POUT),
        cross_barcode_gtperiod([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |L1T],
                               [gtp(TRIAL_PERIOD_ID,
                                    BARCODE_STRING_gtp,
                                    GUEST_ID,
                                    TPERIOD_REDEMPTION_DATE,
                                    TPERIOD_CONSTRAINT_ID,
                                    CANCELED_gtp)   |[]],
                               MOUT),
        merge(POUT,MOUT,FINAL).


% adding one more barcode to an 'already crossing'
cross_barcode_gtperiod(
  [abc(BARCODE_STRING_abc,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |L1T],
  [gtp(TRIAL_PERIOD_ID,
       BARCODE_STRING_gtp,
       GUEST_ID,
       TPERIOD_REDEMPTION_DATE,
       TPERIOD_CONSTRAINT_ID,
       CANCELED_gtp)  |D], % this list needs to be nonempty. the empty case is handled elsewhere
  FINAL ) :-

        t_list_type_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L1T]),
        t_list_type_gtperiod([gtp(TRIAL_PERIOD_ID,
                                  BARCODE_STRING_gtp,
                                  GUEST_ID,
                                  TPERIOD_REDEMPTION_DATE,
                                  TPERIOD_CONSTRAINT_ID,
                                  CANCELED_gtp)   |D]),
        length([abc(BARCODE_STRING_abc,
                    BARCODE_TYPE,
                    AMENITIES_ID,
                    IN_PLAY)   |L1T],
               X),
        X>1,
        length([gtp(TRIAL_PERIOD_ID,
                    BARCODE_STRING_gtp,
                    GUEST_ID,
                    TPERIOD_REDEMPTION_DATE,
                    TPERIOD_CONSTRAINT_ID,
                    CANCELED_gtp)   |D],
               Y),
        Y>1,
        X<Y,
        cross_barcode_gtperiod(L1T,
                               [gtp(TRIAL_PERIOD_ID,
                                    BARCODE_STRING_gtp,
                                    GUEST_ID,
                                    TPERIOD_REDEMPTION_DATE,
                                    TPERIOD_CONSTRAINT_ID,
                                    CANCELED_gtp)   |D],
                               POUT),
        cross_barcode_gtperiod([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |[]],
                               [gtp(TRIAL_PERIOD_ID,
                                    BARCODE_STRING_gtp,
                                    GUEST_ID,
                                    TPERIOD_REDEMPTION_DATE,
                                    TPERIOD_CONSTRAINT_ID,
                                    CANCELED_gtp)   |D],
                               MOUT),
        merge(POUT,MOUT,FINAL).


/*
sweet find: my own customizable printing of terms:

portray(+Term)
    A dynamic predicate, which can be defined by the user to change the behaviour of print/1 on (sub)terms. For each subterm encountered that is not a variable print/1 first calls portray/1 using the term as argument. For lists, only the list as a whole is given to portray/1. If portray/1 succeeds print/1 assumes the term has been written.
*/
