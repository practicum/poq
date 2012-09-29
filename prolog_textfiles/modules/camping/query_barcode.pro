:- module(query_barcode,
          [
           barcode_join_purchase_on_EXPR/3,
           barcode_join_gtperiod_on_EXPR/3,

           % the following predicates are NOT using a 'group by'. instead, it is 'bare aggregation' only.
           aggregate_and_project_1/2,
           aggregate_and_project_1/3,

           end_of_query_barcode_exports_placeholder/0]).   % this is here so i don't have to move the ']).' each time i add to exports


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).
:- use_module(modules/camping/camping).
%:- use_module(modules/dbms/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

end_of_query_barcode_exports_placeholder.


/*
  In structures/functors, using:

  abc - AmenitiesAccessBarcode
  pch - Purchase
  gtp - GuestTrialPeriod
*/

% ----------------------------------------------------------

% ----------------------------------------------------------

/*
There are 7 different clauses to express barcode_join_purchase_on_EXPR.

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

meets_join_abc_pch(
  abc_pch(abc(BARCODE_STRING_abc,
              _BARCODE_TYPE,
              _AMENITIES_ID,
              _IN_PLAY),
          pch(_PURCHASE_ID,
              BARCODE_STRING_pch,
              _PURCHASE_DATE,
              _PURCHASED_SPACES_QTY,
              CANCELED)) ) :-

        BARCODE_STRING_abc = BARCODE_STRING_pch,
        CANCELED = tinyint_0.


barcode_join_purchase_on_EXPR( [], [], [] ).


barcode_join_purchase_on_EXPR(
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


barcode_join_purchase_on_EXPR(
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
barcode_join_purchase_on_EXPR(
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
        t_table_content_purchase([pch(PURCHASE_ID,
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
        meets_join_abc_pch(abc_pch(abc(BARCODE_STRING_abc,
                                       BARCODE_TYPE,
                                       AMENITIES_ID,
                                       IN_PLAY),
                                   pch(PURCHASE_ID,
                                       BARCODE_STRING_pch,
                                       PURCHASE_DATE,
                                       PURCHASED_SPACES_QTY,
                                       CANCELED))),
        barcode_join_purchase_on_EXPR( [abc(BARCODE_STRING_abc,
                                     BARCODE_TYPE,
                                     AMENITIES_ID,
                                     IN_PLAY)   |[]], L2T, R ).


% single barcode but longer list of purchase, FAILS TO MEET JOIN conditions
barcode_join_purchase_on_EXPR(
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
        t_table_content_purchase([pch(PURCHASE_ID,
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
        \+meets_join_abc_pch(abc_pch(abc(BARCODE_STRING_abc,
                                         BARCODE_TYPE,
                                         AMENITIES_ID,
                                         IN_PLAY),
                                     pch(PURCHASE_ID,
                                         BARCODE_STRING_pch,
                                         PURCHASE_DATE,
                                         PURCHASED_SPACES_QTY,
                                         CANCELED))),
        barcode_join_purchase_on_EXPR( [abc(BARCODE_STRING_abc,
                                     BARCODE_TYPE,
                                     AMENITIES_ID,
                                     IN_PLAY)   |[]], L2T, R ).


% longer barcode list but SINGLE purchase, MEETS JOIN conditions
barcode_join_purchase_on_EXPR(
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

        t_table_content_barcode(
            [abc(BARCODE_STRING_abc,
                 BARCODE_TYPE,
                 AMENITIES_ID,
                 IN_PLAY)   |L2T]),
        t_Purchase(
            PURCHASE_ID,
            BARCODE_STRING_pch,
            PURCHASE_DATE,
            PURCHASED_SPACES_QTY,
            CANCELED),
        manageable_list_tail(L2T),
        meets_join_abc_pch(abc_pch(abc(BARCODE_STRING_abc,
                                       BARCODE_TYPE,
                                       AMENITIES_ID,
                                       IN_PLAY),
                                   pch(PURCHASE_ID,
                                       BARCODE_STRING_pch,
                                       PURCHASE_DATE,
                                       PURCHASED_SPACES_QTY,
                                       CANCELED))),
        barcode_join_purchase_on_EXPR( L2T,
                                [pch(PURCHASE_ID,
                                     BARCODE_STRING_pch,
                                     PURCHASE_DATE,
                                     PURCHASED_SPACES_QTY,
                                     CANCELED)   |[]],
                                R ).


% longer barcode list but SINGLE purchase, FAILS TO MEET JOIN conditions
barcode_join_purchase_on_EXPR(
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

        t_table_content_barcode(
            [abc(BARCODE_STRING_abc,
                 BARCODE_TYPE,
                 AMENITIES_ID,
                 IN_PLAY)   |L2T]),
        t_Purchase(
            PURCHASE_ID,
            BARCODE_STRING_pch,
            PURCHASE_DATE,
            PURCHASED_SPACES_QTY,
            CANCELED),
        manageable_list_tail(L2T),
        \+meets_join_abc_pch(abc_pch(abc(BARCODE_STRING_abc,
                                         BARCODE_TYPE,
                                         AMENITIES_ID,
                                         IN_PLAY),
                                     pch(PURCHASE_ID,
                                         BARCODE_STRING_pch,
                                         PURCHASE_DATE,
                                         PURCHASED_SPACES_QTY,
                                         CANCELED))),
        barcode_join_purchase_on_EXPR( L2T,
                                [pch(PURCHASE_ID,
                                     BARCODE_STRING_pch,
                                     PURCHASE_DATE,
                                     PURCHASED_SPACES_QTY,
                                     CANCELED)   |[]],
                                R ).


% adding one more purchase to an 'already crossing'
barcode_join_purchase_on_EXPR(
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

        t_table_content_barcode(
            [abc(BARCODE_STRING_abc,
                 BARCODE_TYPE,
                 AMENITIES_ID,
                 IN_PLAY)   |L1T]),
        t_table_content_purchase(
            [pch(PURCHASE_ID,
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
        barcode_join_purchase_on_EXPR([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |L1T],
                               L2T,
                               POUT),
        barcode_join_purchase_on_EXPR([abc(BARCODE_STRING_abc,
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
barcode_join_purchase_on_EXPR(
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

        t_table_content_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L1T]),
        t_table_content_purchase([pch(PURCHASE_ID,
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
        barcode_join_purchase_on_EXPR(L1T,
                               [pch(PURCHASE_ID,
                                    BARCODE_STRING_pch,
                                    PURCHASE_DATE,
                                    PURCHASED_SPACES_QTY,
                                    CANCELED)   |D],
                               POUT),
        barcode_join_purchase_on_EXPR([abc(BARCODE_STRING_abc,
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
There are 7 different clauses for the next 'cross' predicate.
Refer to the notes above (for barcode_join_purchase_on_EXPR) for more details.
*/
% IMPORTANT. IMPORTANT: roll back to commit cf628e5bc086 to see a 'pure crossing' version with no join conditions

meets_join_abc_gtp(
  abc_gtp(abc(BARCODE_STRING_abc,
              _BARCODE_TYPE,
              _AMENITIES_ID,
              _IN_PLAY),
          gtp(_TRIAL_PERIOD_ID,
              BARCODE_STRING_gtp,
              _GUEST_ID,
              _TPERIOD_REDEMPTION_DATE,
              _TPERIOD_CONSTRAINT_ID,
              CANCELED_gtp)) ) :-

        BARCODE_STRING_abc = BARCODE_STRING_gtp,
        CANCELED_gtp = tinyint_0.


barcode_join_gtperiod_on_EXPR( [], [], [] ).


barcode_join_gtperiod_on_EXPR(
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


barcode_join_gtperiod_on_EXPR(
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


% single barcode but longer list of gtperiod(s), and MEETS join condition
barcode_join_gtperiod_on_EXPR(
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
        t_table_content_gtperiod([gtp(TRIAL_PERIOD_ID,
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
        meets_join_abc_gtp(abc_gtp(abc(BARCODE_STRING_abc,
                                       BARCODE_TYPE,
                                       AMENITIES_ID,
                                       IN_PLAY),
                                   gtp(TRIAL_PERIOD_ID,
                                       BARCODE_STRING_gtp,
                                       GUEST_ID,
                                       TPERIOD_REDEMPTION_DATE,
                                       TPERIOD_CONSTRAINT_ID,
                                       CANCELED_gtp))),
        barcode_join_gtperiod_on_EXPR( [abc(BARCODE_STRING_abc,
                                     BARCODE_TYPE,
                                     AMENITIES_ID,
                                     IN_PLAY)   |[]], L2T, R ).


% single barcode but longer list of gtperiod(s), and FAILING the join condition
barcode_join_gtperiod_on_EXPR(
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
  R ) :-

        t_AmenitiesAccessBarcode(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        t_table_content_gtperiod([gtp(TRIAL_PERIOD_ID,
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
        \+meets_join_abc_gtp(abc_gtp(abc(BARCODE_STRING_abc,
                                         BARCODE_TYPE,
                                         AMENITIES_ID,
                                         IN_PLAY),
                                     gtp(TRIAL_PERIOD_ID,
                                         BARCODE_STRING_gtp,
                                         GUEST_ID,
                                         TPERIOD_REDEMPTION_DATE,
                                         TPERIOD_CONSTRAINT_ID,
                                         CANCELED_gtp))),
        barcode_join_gtperiod_on_EXPR( [abc(BARCODE_STRING_abc,
                                     BARCODE_TYPE,
                                     AMENITIES_ID,
                                     IN_PLAY)   |[]], L2T, R ).


% longer barcode list but SINGLE gtperiod, and MEETS the join condition
barcode_join_gtperiod_on_EXPR(
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

        t_table_content_barcode([abc(BARCODE_STRING_abc,
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
        meets_join_abc_gtp(abc_gtp(abc(BARCODE_STRING_abc,
                                       BARCODE_TYPE,
                                       AMENITIES_ID,
                                       IN_PLAY),
                                   gtp(TRIAL_PERIOD_ID,
                                       BARCODE_STRING_gtp,
                                       GUEST_ID,
                                       TPERIOD_REDEMPTION_DATE,
                                       TPERIOD_CONSTRAINT_ID,
                                       CANCELED_gtp))),
        barcode_join_gtperiod_on_EXPR( L2T,
                                [gtp(TRIAL_PERIOD_ID,
                                     BARCODE_STRING_gtp,
                                     GUEST_ID,
                                     TPERIOD_REDEMPTION_DATE,
                                     TPERIOD_CONSTRAINT_ID,
                                     CANCELED_gtp)   |[]],
                                R ).


% longer barcode list but SINGLE gtperiod, and FAILING the join condition
barcode_join_gtperiod_on_EXPR(
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
  R ) :-

        t_table_content_barcode([abc(BARCODE_STRING_abc,
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
        \+meets_join_abc_gtp(abc_gtp(abc(BARCODE_STRING_abc,
                                         BARCODE_TYPE,
                                         AMENITIES_ID,
                                         IN_PLAY),
                                     gtp(TRIAL_PERIOD_ID,
                                         BARCODE_STRING_gtp,
                                         GUEST_ID,
                                         TPERIOD_REDEMPTION_DATE,
                                         TPERIOD_CONSTRAINT_ID,
                                         CANCELED_gtp))),
        barcode_join_gtperiod_on_EXPR( L2T,
                                [gtp(TRIAL_PERIOD_ID,
                                     BARCODE_STRING_gtp,
                                     GUEST_ID,
                                     TPERIOD_REDEMPTION_DATE,
                                     TPERIOD_CONSTRAINT_ID,
                                     CANCELED_gtp)   |[]],
                                R ).


% adding one more gtperiod to an 'already crossing'
barcode_join_gtperiod_on_EXPR(
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

        t_table_content_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L1T]),
        t_table_content_gtperiod([gtp(TRIAL_PERIOD_ID,
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
        barcode_join_gtperiod_on_EXPR([abc(BARCODE_STRING_abc,
                                    BARCODE_TYPE,
                                    AMENITIES_ID,
                                    IN_PLAY)   |L1T],
                               L2T,
                               POUT),
        barcode_join_gtperiod_on_EXPR([abc(BARCODE_STRING_abc,
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
barcode_join_gtperiod_on_EXPR(
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

        t_table_content_barcode([abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY)   |L1T]),
        t_table_content_gtperiod([gtp(TRIAL_PERIOD_ID,
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
        barcode_join_gtperiod_on_EXPR(L1T,
                               [gtp(TRIAL_PERIOD_ID,
                                    BARCODE_STRING_gtp,
                                    GUEST_ID,
                                    TPERIOD_REDEMPTION_DATE,
                                    TPERIOD_CONSTRAINT_ID,
                                    CANCELED_gtp)   |D],
                               POUT),
        barcode_join_gtperiod_on_EXPR([abc(BARCODE_STRING_abc,
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

% ----------------------------------------------------------

/* we are aggregating rows that look like:

abc_pch(abc(BARCODE_STRING_abc,
                 BARCODE_TYPE,
                 AMENITIES_ID,
                 IN_PLAY),
             pch(PURCHASE_ID,
                 BARCODE_STRING_pch,
                 PURCHASE_DATE,
                 PURCHASED_SPACES_QTY,
                 CANCELED))
*/

% this is aggregation WITHOUT (aka IN THE ABSENCE OF) any group-by.

% still todo: must account for NULL values in all aggregates.

/*
  here is the aggregated projected result:

  count(amenities_id)
  min(amenities_id)
  max(amenities_id)
  min(barcode_type)
  max(barcode_type)

  we will call it ct1 for custom tuple 1 (or custom tuple-type 1)
*/



/*
  note: the final map can be examined with: assoc_to_list, assoc_to_values

  possibly also with failure-driven backtracking:
    gen_assoc(?Key, +Assoc, ?Value)
      Enumerate matching elements of Assoc in ascending order of their keys via backtracking.
*/
aggregate_and_project_1(L,LOUT) :-

        t_dt_content_barcode_x_purchase(L),
        aggregate_and_project_1(L,t,LOUT).


% pared everything down to an empty list, but there is also an EMPTY MAP (the so-far map)
aggregate_and_project_1([],t,MAP) :-
        put_assoc(absent_group_by,
                  t,
                  ct1(0,        % count(amenities_id)
                      null,     % min(amenities_id)
                      null,     % max(amenities_id)
                      null,     % min(barcode_type)
                      null),    % max(barcode_type)
                  MAP).

% nothing in the list for further processing. so your 'map so-far' is your finished map.
aggregate_and_project_1([],MAP,MAP) :-
        \+empty_assoc(MAP).  % make sure it is NOT an empty map, because that case is handled elsewhere

% take the list-of-tuples, our 'so-far' map, and produce a done-map.
aggregate_and_project_1(
  [abc_pch(abc(BARCODE_STRING_abc,
               BARCODE_TYPE,
               AMENITIES_ID,
               IN_PLAY),
           pch(PURCHASE_ID,
               BARCODE_STRING_pch,
               PURCHASE_DATE,
               PURCHASED_SPACES_QTY,
               CANCELED))   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),
        t_tuple_abc_pch(abc(BARCODE_STRING_abc,
                            BARCODE_TYPE,
                            AMENITIES_ID,
                            IN_PLAY),
                        pch(PURCHASE_ID,
                            BARCODE_STRING_pch,
                            PURCHASE_DATE,
                            PURCHASED_SPACES_QTY,
                            CANCELED)),

        get_assoc(absent_group_by, % map key. this indicates we could replace map here with a simpler accumulator
                  MAP,
                  ct1(CNT_AM_ID_2,    % count(amenities_id)
                      MIN_AM_ID_2,    % min(amenities_id)
                      MAX_AM_ID_2,    % max(amenities_id)
                      MIN_BTYPE_2,    % min(barcode_type)
                      MAX_BTYPE_2) ), % max(barcode_type)

        agg_field_count(    CNT_AM_ID_2,  AMENITIES_ID,  AMID_NEW),
        agg_field_min_atom( MIN_AM_ID_2,  AMENITIES_ID,  MIN_AMID_NEW),
        agg_field_max_atom( MAX_AM_ID_2,  AMENITIES_ID,  MAX_AMID_NEW),
        agg_field_min_atom( MIN_BTYPE_2,  BARCODE_TYPE,  MIN_BTYPE_NEW),
        agg_field_max_atom( MAX_BTYPE_2,  BARCODE_TYPE,  MAX_BTYPE_NEW),

        put_assoc(absent_group_by,
                  MAP,
                  ct1(AMID_NEW,        % count(amenities_id)
                      MIN_AMID_NEW,    % min(amenities_id)
                      MAX_AMID_NEW,    % max(amenities_id)
                      MIN_BTYPE_NEW,   % min(barcode_type)
                      MAX_BTYPE_NEW),  % max(barcode_type)
                  MAP2),

        aggregate_and_project_1(LT,MAP2,MAP_OUT).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
% This is the case where the GROUP KEY is not yet in the map.
aggregate_and_project_1(
  [abc_pch(abc(BARCODE_STRING_abc,
               BARCODE_TYPE,
               AMENITIES_ID,
               IN_PLAY),
           pch(PURCHASE_ID,
               BARCODE_STRING_pch,
               PURCHASE_DATE,
               PURCHASED_SPACES_QTY,
               CANCELED))   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),

        t_tuple_abc_pch(abc(BARCODE_STRING_abc,
                            BARCODE_TYPE,
                            AMENITIES_ID,
                            IN_PLAY),
                        pch(PURCHASE_ID,
                            BARCODE_STRING_pch,
                            PURCHASE_DATE,
                            PURCHASED_SPACES_QTY,
                            CANCELED)),

        \+get_assoc(absent_group_by,MAP,_), % map key. this indicates we could replace map here with a simpler accumulator

        % we probably want 'aggregation base case' predicates here.
        % perhaps especially when null comes into play? or not...
        % or better: perhaps in the face of agg_field_do_nothing

        put_assoc(absent_group_by,
                  MAP,
                  ct1(1,             % note: 1 is the starting point for the count aggregate
                      AMENITIES_ID,  % min(amenities_id)
                      AMENITIES_ID,  % max(amenities_id)
                      BARCODE_TYPE,  % min(barcode_type)
                      BARCODE_TYPE), % max(barcode_type)
                  MAP2),
        aggregate_and_project_1(LT,MAP2,MAP_OUT).


% ----------------------------------------------------------
