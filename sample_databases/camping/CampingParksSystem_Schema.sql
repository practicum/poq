


CREATE TABLE IF NOT EXISTS PayingMember
(
    paying_member_id bigint unsigned NOT NULL UNIQUE auto_increment,

    member_email varchar(255)  NOT NULL UNIQUE,
    password varchar(255) NOT NULL,
    personal_data_id bigint unsigned NOT NULL default 1,

    PRIMARY KEY  (paying_member_id),
    KEY  (member_email)

) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;

-- the reason why we prefer a default of 1 for column personal_data_id
-- is that we rely on an assumption that when we insert the very first
-- row into PersonalData, it will receive an id of 1 via
-- auto_increment.  And that very first row we insert into
-- PersonalData is a 'dummy/pseudo-null' type of placeholder row. Is
-- is safe to make the assumption? Yes, according to
-- http://dev.mysql.com/doc/refman/5.0/en/innodb-auto-increment-handling.html
-- , InnoDB increments by one the value retrieved by the statement and
-- assigns it to the column and to the auto-increment counter for the
-- table. If the table is empty, InnoDB uses the value 1.
CREATE TABLE IF NOT EXISTS InvitedGuest
(
    guest_id bigint unsigned NOT NULL UNIQUE auto_increment,
    -- The name "guest_email" is used so that no one will be misled
    -- into thinking that "member_email" (from the PayingMember
    -- table) is a foreign key into InvitedGuest, or vice versa.  If
    -- the PayingMember table and the InvitedGuest each had a column
    -- identically named "email_address", then it would misleadingly
    -- indicate that they key into the same data or share some
    -- overlapping purpose.
    guest_email varchar(255)  NOT NULL ,
    personal_data_id bigint unsigned NOT NULL default 1,
    PRIMARY KEY (guest_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;


INSERT INTO InvitedGuest ( guest_email ) values ( '{{null guest for the null invited membership period}}' );

CREATE TABLE IF NOT EXISTS PersonalData
(
    personal_data_id bigint unsigned NOT NULL UNIQUE auto_increment,
    fullname varchar(255)  NOT NULL default '{{unknown}}',
    organization varchar(255)  NOT NULL default '{{unknown}}',
    PRIMARY KEY (personal_data_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;


-- Populate the PersonalData table with our first required default row
INSERT INTO PersonalData ( ) values ( ); -- NOTE: 'fullname' and 'organization' will receive the defaults


-- for membership types such as 'golf club country club access' and 'river/lake dock access'
CREATE TABLE IF NOT EXISTS AmenitiesAccessType
(
    amenities_id bigint unsigned NOT NULL UNIQUE auto_increment,
    amenities_access_level_name varchar(255) NOT NULL default '{{unknown}}',
    PRIMARY KEY (amenities_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;


-- Populate the AmenitiesAccessType table with our first required default row
INSERT INTO AmenitiesAccessType ( ) values ( );


CREATE TABLE IF NOT EXISTS AmenitiesAccessBarcode
(
    barcode_string varchar(255) NOT NULL UNIQUE ,
    barcode_type ENUM('FULL_MEMBER_BARCODE', 'GUEST_BARCODE') NOT NULL default 'FULL_MEMBER_BARCODE',
    amenities_id bigint unsigned NOT NULL default 1, -- The default of '1' will link this to the AmenitiesAccessType named '{{unknown}}', which is fine.
    in_play TINYINT(1) NOT NULL DEFAULT 0, -- Only really matters for guest barcodes. Once any barcode has any rows in Purchase or in GuestTrialPeriod, then in_play should be TRUE (TRUE is represented as '1')

        -- NOTE: "in_play" is to help us with CONCURRENCY.  This is
        --    something we set to true to "CLAIM" the barcode so that
        --    only OUR PROCESS will issue it. otherwise, two
        --    concurrent database connections could "select all
        --    not-in-play barcodes" and then they could both
        --    simultaneously decide to choose the SAME available
        --    barcode as a barcode to issue during a sale or trial membership.

        --    The combined use of this in_play column together with the
        --    built-in locking features of the DBMS allow us to write a
        --    single, straightforward query that locates appropriate 'fresh'
        --    barcodes to use for a purchase or trial membership, and we
        --    achieve safety via an "all-at-once" Test-And-Set operation on
        --    in_play *while* retrieving the
        --    barcodes. http://en.wikipedia.org/wiki/Test-and-set . The use of
        --    in_play and built-in features of SQL allow this to be written as
        --    one straightforward query with no explicit 'LOCK' commands.
        --    (refer also the application code)

        --    PLEASE DO NOT USE "in_play" FOR ANYTHING OTHER THAN
        --    CONCURRENCY RESOLUTION do **not** **not** **not** use
        --    "in_play" as a SUBSTITUTE for actually checking the
        --    Purchase table or the GuestTrialPeriod table if what you
        --    really are trying to accomplish is to simply SEE IF A
        --    BARCODE WAS ISSUED TO A CUSTOMER.

        --    Again... in_play is ONLY for concurrency solutions and
        --    should not be used in "INFORMATIONAL" queries.

    PRIMARY KEY (barcode_string)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;


CREATE TABLE IF NOT EXISTS BarcodeMemberJoin
(
    barcode_string varchar(255) NOT NULL ,
    paying_member_id bigint unsigned NOT NULL,
    assignment_date DATETIME NOT NULL DEFAULT 0,
    canceled TINYINT(1) NOT NULL DEFAULT 0, -- This cancels the idea that this particular member uses this barcode, but it does not cancel the barcode itself outright.  To fully remove a barcode from being valid, you need to cancel all Purchase(s) of the barcode.

    PRIMARY KEY (barcode_string, paying_member_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;



CREATE TABLE IF NOT EXISTS Purchase
(
    --  "purchase_id" is not really a primary key in the typical sense. It is here as a convenience unique row id.
    --  Nonetheless, it is possible to have two rows such that all their column values (except for the column "purchase_id") match identically.
    --  Usually, however, we would expect only one purchase per barcode_string per purchase_date.  But we cannot harshly restrict it to that.
    purchase_id bigint unsigned NOT NULL UNIQUE auto_increment,
    barcode_string varchar(255) NOT NULL ,
    purchase_date DATETIME NOT NULL DEFAULT 0,
    purchased_copies_qty int NOT NULL DEFAULT 0,
    canceled TINYINT(1) NOT NULL DEFAULT 0
                                --
                                -- NOTE THE DIFFERENCE BETWEEN CANCELING A Purchase AND CANCELING A BarcodeMemberJoin :
                                --
                                -- Only touch BarcodeMemberJoin.canceled if you want to
                                -- TRANSFER OWNERSHIP.  If you want to "cancel" or "revoke"
                                -- ownership of a barcode, then you should use the Purchase table
                                -- to cancel the Purchase.  At that point you could also
                                -- harmlessly set BarcodeMemberJoin.canceled, but that would
                                -- be redundant and it is IMPORTANT to set Purchase.canceled
                                -- first and foremost.  Also note that there can be MORE THAN
                                -- ONE Purchase row allocating more registered vehicles to the same
                                -- barcode several times in various rows.  This is because if a
                                -- customer previously bought a barcode and paid for 10 quantity
                                -- (think "10 vehicles" .. NOT 10 barcode strings), then if
                                -- that customer comes back and says "i want to buy 10 more",
                                -- then we create more purchases, but still linked to the
                                -- SAME original barcode.  So, you get FINER-GRAINED control when
                                -- you do cancellations of purchases.

--    PRIMARY KEY (purchase_id)  --This is intentionally commented out. This table doesn't exactly have a primary key. Everything in here is like a bookkeeping log.

) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;





CREATE TABLE IF NOT EXISTS GuestTrialPeriod
(
    --  "trial_period_id" is not really a primary key in the typical sense. It is here as a convenience unique row id.
    --  Nonetheless, it is possible to have two rows such that all their column values (except for the column "trial_period_id") match identically.
    trial_period_id bigint unsigned NOT NULL UNIQUE auto_increment,
    barcode_string varchar(255) NOT NULL , -- must be a barcode of type GUEST_BARCODE
    guest_id bigint unsigned NOT NULL,
    tperiod_redemption_date DATETIME NOT NULL DEFAULT 0,
    tperiod_constraint_id bigint unsigned NOT NULL ,
    canceled TINYINT(1) NOT NULL DEFAULT 0

--    PRIMARY KEY (trial_period_id)  --This is intentionally commented out. This table doesn't exactly have a primary key.
-- **DESPITE** the preceding line that has a COMMENTED-OUT declaration of a p.k., we might want to INDEX on trial_period_id.
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;

-- Populate the GuestTrialPeriod table with our first required default row
INSERT INTO GuestTrialPeriod ( barcode_string, guest_id,  tperiod_constraint_id ) values ( '{{default null trial period row}}', 1, 0 ); -- that value of guest_id=1 resolves to ''{{null guest for the null invited membership period}}', search above in this file.




CREATE TABLE IF NOT EXISTS GuestTrialPeriodConstraint
(
    tperiod_constraint_id bigint unsigned NOT NULL UNIQUE auto_increment,
    barcode_string varchar(255) NOT NULL , -- must be a barcode of type FULL_MEMBER_BARCODE
    tperiod_invitation_date DATETIME NOT NULL DEFAULT 0,
    duration_length_in_days bigint unsigned NOT NULL DEFAULT 0,
    duration_type ENUM('STARTS_FROM_INVITATION_DATE', 'STARTS_FROM_REDEMPTION_DATE') NOT NULL default 'STARTS_FROM_REDEMPTION_DATE',
    lag_days_allowed_from_invitation_til_redemption bigint unsigned NOT NULL DEFAULT 0,

    PRIMARY KEY (tperiod_constraint_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;



-- ############ EXPLANATION OF THE "CLASSES" OF ROWS YOU CAN FIND IN THE VehicleRegistration TABLE ######################
-- ##
-- ##  Note: Everywhere you see the number '1' over the next few lines, '1' is a FLAG value analogous to NULL
-- ##
-- ##  Note: 'VR' is being used here as an abbreviation for table name VehicleRegistration
-- ##
-- ##  VR.vehicle_id = 1,     VR.trial_period_id = 1                -- SHOULD NOT EVER HAPPEN !!!!!!
-- ##
-- ##  VR.vehicle_id = 1,     VR.trial_period_id = greater-than-1   -- this is a pending, not-yet-redeemed trial.
-- ##                               Therefore, REGISTRATION_DATE must be ZERO. (expiration_date will be MAX possible date)
-- ##
-- ##  VR.vehicle_id = greater-than-1,   VR.trial_period_id = 1     -- this is a FULL MEMBER REGISTRATION.
-- ##                               Therefore, expiration_date MUST BE ZERO. REGISTRATION_DATE must be a VALID DATE.
-- ##
-- ##  VR.vehicle_id = greater-than-1,   VR.trial_period_id = greater-than-1   -- this is a TRIAL REGISTRATION.
-- ##          Therefore, REGISTRATION_DATE must be a VALID DATE, and expiration_date must be valid.
-- ##
-- ######################################################################################################################


-- ########### -- Another way to visualize the classes of rows in the VehicleRegistration table:  ###################################
--
--                                      values for trial_period_id
--                            -------------------------------------------
--
--                                   == 1                 >  1
--                     |      -------------------------------------------
--                     |      |                   |                     |
--                     |      |  Reject. This     |   Pending trial.    |
--                     |      |  is an invalid    |   This row shows    |
--                     | == 1 |  state that no    |   that an invite    |
--                     |      |  rows should      |   was issued but not|
--           values    |      |  have             |   yet redeemed.     |
--           for       |      -------------------------------------------
--           vehicle_id|      |                   |                     |
--                     |      |  This is a        |   This is a trial   |
--                     |      |  full member      |   that has been     |
--                     | > 1  |  registration.    |   redeemed by a     |
--                     |      |  Does not expire. |   guest.            |
--                     |      |                   |                     |
--                     |      |                   |                     |
--                     |      -------------------------------------------
--
--
--       NOTE:    A "pending trial" **can** be both pending (never redeemed) **and** expired. Pending trials do expire, due to GuestTrialPeriodConstraint.lag_days_allowed_from_invitation_til_redemption
--
-- ###########################################################################################################################

-- VERY IMPORTANT !!!   There are sql statements in the application that DEPEND ON THE FACT THAT VehicleRegistration does **NOT** have any column of type "auto_increment".
-- DO NOT ADD ANY auto_increment COLUMN TO THIS TABLE WITHOUT CAREFUL CONSIDERATION OF HOW THAT WILL AFFECT THE application.
CREATE TABLE IF NOT EXISTS VehicleRegistration
(
    barcode_string varchar(255) NOT NULL , -- must be a barcode of type FULL_MEMBER_BARCODE
    vehicle_id bigint unsigned NOT NULL DEFAULT 1, -- vehicle_id of '1' is the NULL vehicle. this happens when a TRIAL was created but NOT YET REDEEMED.

    trial_period_id bigint unsigned NOT NULL DEFAULT 1, -- trial_period_id of '1' indicates to us that this is NOT a trial. there will be MANY rows with trial_period_id = 1
    registration_date DATETIME NOT NULL DEFAULT 0,
    expiration_date DATETIME NOT NULL DEFAULT 0, -- if a vehicle was registered with a GUEST barcode, then the registration **will** expire

    canceled TINYINT(1) NOT NULL DEFAULT 0,

    PRIMARY KEY (barcode_string, vehicle_id, trial_period_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;





CREATE TABLE IF NOT EXISTS Vehicle
(
    vehicle_id bigint unsigned NOT NULL UNIQUE auto_increment,
    vehicle_textual_description varchar(255) NOT NULL ,

    PRIMARY KEY (vehicle_id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;


-- Populate the Vehicle table with our first required default row
INSERT INTO Vehicle ( vehicle_textual_description ) values ( '{{default null vehicle}}' );



CREATE TABLE IF NOT EXISTS VehicleTraits
(
    vehicle_trait_id bigint unsigned NOT NULL UNIQUE auto_increment,
    vehicle_id bigint unsigned NOT NULL,
    trait_name varchar(255) NOT NULL ,
    trait_value varchar(255) NOT NULL ,

    PRIMARY KEY (vehicle_id, trait_name, trait_value)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE utf8_general_ci;


