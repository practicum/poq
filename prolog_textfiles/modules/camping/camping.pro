:- module(camping,
          [t_AmenitiesAccessBarcode/4,     % simple, straightforward tuple type predicate.
           t_Purchase/5,                   % simple, straightforward tuple type predicate.
           t_GuestTrialPeriod/6,           % simple, straightforward tuple type predicate.
           t_AmenitiesAccessType/2,        % simple, straightforward tuple type predicate.

           t_table_content_barcode/1,      % valid tuples populating AmenitiesAccessBarcode. encapsulates UNIQUE constraint.
           t_table_content_purchase/1,     % valid tuples populating Purchase. encapsulates UNIQUE constraint.
           t_table_content_gtperiod/1,     % valid tuples populating GuestTrialPeriod. encapsulates UNIQUE constraint.

           test_group_by/2,
           test_group_by/3,

           test_bare_aggregate/2,
           test_bare_aggregate/3,

           t_tuple_abc_pch/1,              % type of tuple when joining AmenitiesAccessBarcode to Purchase.
           t_tuple_gtp_pch/1,              % type of tuple when joining GuestTrialPeriod to Purchase.

           % 'dt' is used as an abbreviation for DERIVED TABLE.
           t_dt_content_barcode_x_purchase/1,   % despite the specific kind of join we do on these 2 tables, the output type is this.
           t_dt_content_gtperiod_x_purchase/1,  % despite the specific kind of join we do on these 2 tables, the output type is this.

           end_of_camping_exports_placeholder/0]).   % this is here so i don't have to move the ']).' each time i add to exports


:- use_module(modules/dbms/small_lists).
:- use_module(modules/dbms/dbms_builtins).
%:- use_module(modules/dbms/datatypes).  NO. DO NOT ENABLE. instead, the user imports ONE of several choices.

end_of_camping_exports_placeholder.


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





% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
t_table_content_barcode(L) :-
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

% we are grouping rows that look like: abc(fccy463, full_member_barcode, 0, tinyint_0)
% we are grouping based on BARCODE_TYPE. (the second field in the row)

% still todo: must account for NULL values in all aggregates.

% the aggregate function for field 1 is nothing-at-all.
% the aggregate function for BARCODE_TYPE (the GROUP_KEY) is count. handled specially since it is GROUP_KEY.
% the aggregate function for AMENITIES_ID is sum. (it makes no sense to sum a key, but this is a demo.)
% the aggregate function for IN_PLAY is min.

/*
  note: the final map can be examined with: assoc_to_list, assoc_to_values

  possibly also with failure-driven backtracking:
    gen_assoc(?Key, +Assoc, ?Value)
      Enumerate matching elements of Assoc in ascending order of their keys via backtracking.
*/
test_group_by(L,LOUT) :-

        t_table_content_barcode(L),
        test_group_by(L,t,LOUT).


% nothing in the list for further processing. so your 'map so-far' is your finished map.
test_group_by([],MAP,MAP).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
test_group_by(
  [abc(BARCODE_STRING,
       GROUP_KEY,               %BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),
        t_AmenitiesAccessBarcode(BARCODE_STRING,
                                 GROUP_KEY, %BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        get_assoc(GROUP_KEY, % map key (GROUP_KEY) needs to be instantiated by here.
                  MAP,
                  g(abc(BARCODE_STRING2,
                        GROUP_KEY, %BARCODE_TYPE,
                        AMENITIES_ID2,
                        IN_PLAY2),COUNT) ),

        agg_field_do_nothing(BARCODE_STRING2,BARCODE_STRING,BSNEW),
        agg_field_sum(AMENITIES_ID2,AMENITIES_ID,AIDNEW),
        agg_field_min_atom(IN_PLAY2,IN_PLAY,IPNEW),
        NEW_COUNT is COUNT + 1,

        put_assoc(GROUP_KEY,
                  MAP,
                  g(abc(BSNEW,
                        GROUP_KEY, %BARCODE_TYPE,
                        AIDNEW,
                        IPNEW),
                    NEW_COUNT),
                  MAP2),

        test_group_by(LT,MAP2,MAP_OUT).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
test_group_by(
  [abc(BARCODE_STRING,
       GROUP_KEY,               %BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),
        t_AmenitiesAccessBarcode(BARCODE_STRING,
                                 GROUP_KEY, %BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        \+get_assoc(GROUP_KEY,MAP,_), % map key (GROUP_KEY) needs to be instantiated by here.
        put_assoc(GROUP_KEY,
                  MAP,
                  g(abc(BARCODE_STRING,  % aggregate: nothing-at-all
                        GROUP_KEY, %BARCODE_TYPE,
                        AMENITIES_ID,    % aggregate: sum
                        IN_PLAY),  % aggregate: min
                    1), % note: 1 is the starting point for the count aggregate
                  MAP2),
        test_group_by(LT,MAP2,MAP_OUT).


% ----------------------------------------------------------

% we are aggregating rows that look like: abc(fccy463, full_member_barcode, 0, tinyint_0)
% this is aggregation WITHOUT (aka IN THE ABSENCE OF) any group-by.

% still todo: must account for NULL values in all aggregates.

% the aggregate function for field 1 is nothing-at-all.
% the aggregate function for BARCODE_TYPE is nothing-at-all.
% the aggregate function for AMENITIES_ID is sum. (it makes no sense to sum a key, but this is a demo.)
% the aggregate function for IN_PLAY is min.


/*
  note: the final map can be examined with: assoc_to_list, assoc_to_values

  possibly also with failure-driven backtracking:
    gen_assoc(?Key, +Assoc, ?Value)
      Enumerate matching elements of Assoc in ascending order of their keys via backtracking.
*/
test_bare_aggregate(L,LOUT) :-

        t_table_content_barcode(L),
        test_bare_aggregate(L,t,LOUT).


% pared everything down to an empty list, but there is also an EMPTY MAP (the so-far map)
test_bare_aggregate([],t,MAP) :-
        put_assoc(sole_item,
                  t,
                  g(abc(null, %BARCODE_STRING,  % aggregate: nothing-at-all
                        null, %BARCODE_TYPE,    % aggregate: nothing-at-all
                        null, %AMENITIES_ID,    % aggregate: sum
                        null), % IN_PLAY  % aggregate: min
                    0), % the empty tuple list has a count of zero
                  MAP).

% nothing in the list for further processing. so your 'map so-far' is your finished map.
test_bare_aggregate([],MAP,MAP) :-
        \+empty_assoc(MAP).  % make sure it is NOT an empty map, because that case is handled elsewhere

% take the list-of-tuples, our 'so-far' map, and produce a done-map.
test_bare_aggregate(
  [abc(BARCODE_STRING,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),
        t_AmenitiesAccessBarcode(BARCODE_STRING,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        get_assoc(sole_item, % map key. this indicates we could replace map here with a simpler accumulator
                  MAP,
                  g(abc(BARCODE_STRING2,
                        BARCODE_TYPE2,
                        AMENITIES_ID2,
                        IN_PLAY2),COUNT) ),

        agg_field_do_nothing(BARCODE_STRING2,BARCODE_STRING,BSNEW),
        agg_field_do_nothing(BARCODE_TYPE2,BARCODE_TYPE,BCNEW),
        agg_field_sum(AMENITIES_ID2,AMENITIES_ID,AIDNEW),
        agg_field_min_atom(IN_PLAY2,IN_PLAY,IPNEW),
        NEW_COUNT is COUNT + 1,

        put_assoc(sole_item,
                  MAP,
                  g(abc(BSNEW,
                        BCNEW,
                        AIDNEW,
                        IPNEW),
                    NEW_COUNT),
                  MAP2),

        test_bare_aggregate(LT,MAP2,MAP_OUT).


% take the list-of-tuples, our 'so-far' map, and produce a done-map.
test_bare_aggregate(
  [abc(BARCODE_STRING,
       BARCODE_TYPE,
       AMENITIES_ID,
       IN_PLAY)   |LT],
  MAP,
  MAP_OUT ) :-

        manageable_list_tail(LT),
        t_AmenitiesAccessBarcode(BARCODE_STRING,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
        \+get_assoc(sole_item,MAP,_), % map key. this indicates we could replace map here with a simpler accumulator
        put_assoc(sole_item,
                  MAP,
                  g(abc(BARCODE_STRING,  % aggregate: nothing-at-all
                        BARCODE_TYPE,    % aggregate: nothing-at-all
                        AMENITIES_ID,    % aggregate: sum
                        IN_PLAY),  % aggregate: min
                    1), % note: 1 is the starting point for the count aggregate
                  MAP2),
        test_bare_aggregate(LT,MAP2,MAP_OUT).


% ----------------------------------------------------------

% putting the UNIQUE barcode_string information here.  TODO: what if two columns bore the unique keyword?
t_table_content_purchase(L) :-
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
t_table_content_gtperiod(L) :-
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

% type definition for a tuple from crossing AmenitiesAccessBarcode(s) with Purchase(s)
t_tuple_abc_pch(
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

% the list to stand in for a 'set' of tuples of type t_tuple_abc_pch
t_dt_content_barcode_x_purchase([]).


t_dt_content_barcode_x_purchase(
  [abc_pch(abc(BARCODE_STRING_abc,
                 BARCODE_TYPE,
                 AMENITIES_ID,
                 IN_PLAY),
             pch(PURCHASE_ID,
                 BARCODE_STRING_pch,
                 PURCHASE_DATE,
                 PURCHASED_SPACES_QTY,
                 CANCELED))   |LT]) :-

        t_tuple_abc_pch(abc(BARCODE_STRING_abc,
                                 BARCODE_TYPE,
                                 AMENITIES_ID,
                                 IN_PLAY),
                             pch(PURCHASE_ID,
                                 BARCODE_STRING_pch,
                                 PURCHASE_DATE,
                                 PURCHASED_SPACES_QTY,
                                 CANCELED)),
        manageable_list_tail(LT), % it is very important to put this size PRIOR to the recursion below
        t_dt_content_barcode_x_purchase(LT).


% ----------------------------------------------------------


% type definition for a tuple from crossing GuestTrialPeriod(s) with Purchase(s)
t_tuple_gtp_pch(
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


% the list to stand in for a 'set' of tuples of type t_tuple_gtp_pch
t_dt_content_gtperiod_x_purchase([]).


t_dt_content_gtperiod_x_purchase(
  [gtp_pch(gtp(TRIAL_PERIOD_ID,
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

        t_tuple_gtp_pch(gtp(TRIAL_PERIOD_ID,
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
        t_dt_content_gtperiod_x_purchase(LT).


% ----------------------------------------------------------
