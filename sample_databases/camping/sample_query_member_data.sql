
-- NOTE: this concept of 'pseudonull_date' was introduced because the
-- application code could not handle 'zero dates' that the DBMS does
-- handle (the ones that look like this in mysql: '0000-00-00 00:00:00'
select '1970-01-01' into @pseudonull_date;
select '1970-06-01' into @minimum_date;
select '2200-01-01' into @maximum_date;


SELECT
    -- We do a group by on (barcode, vehicle, trial id) as a SANITY
    -- CHECK. Those three fields should UNIQUELY identify one
    -- VehicleRegistration.  Therefore, if we ever get MORE THAN ONE
    -- row at a time that can POSSIBLY be aggregated together under
    -- this clause, then something is really wrong (either we have
    -- corrupt data or a bad SQL query or both)

      count(*)                                          as error_if_not_one
    , access.amenities_access_level_name                as access_name
    , member_owned_barcodes.b_str                       as full_member_barcode
    , member_owned_barcodes.access_id                   as a_id
    , vreg.vehicle_id                                   as v_id -- if trial_period_id > 1, then this should ALWAYS be > 1
    , vreg.trial_period_id                              as t_id
    , coalesce(guest.guest_id,0)                        as g_id
    , IF(   vreg.trial_period_id = 1
            , 'full paid membership'
            , 'trial membership' )                      as reg_type

    , coalesce( veh.vehicle_textual_description, '' )   as v_description

    -- NOTE: further application code can decide whether to HIDE or SHOW the trial barcode.
    --   The code should be HIDDEN (just show a blank string) if this is a FULL registration.
    , coalesce(trial.barcode_string ,'')                as trial_barcode
    , coalesce(guest.guest_email, '')                   as guest_addr

    -- NOTE: further application code can decide whether to HIDE or SHOW the registration date.
    --   The date should be HIDDEN (just show a blank string) if this is a PENDING trial.
    , IF ( vreg.registration_date > @minimum_date, IF ( vreg.registration_date < @maximum_date, vreg.registration_date , @pseudonull_date ), @pseudonull_date)  as v_registration_date

    -- NOTE: further application code can decide whether to HIDE or SHOW the expiration date.
    --   The date should be HIDDEN (just show a blank string) if this is a FULL registration.
    , IF ( vreg.expiration_date > @minimum_date, IF ( vreg.expiration_date < @maximum_date, vreg.expiration_date , @pseudonull_date ), @pseudonull_date)  as expiration_date

FROM
(
    SELECT
        bmj.barcode_string            as b_str
        , IF ( bmj.assignment_date > @minimum_date, IF ( bmj.assignment_date < @maximum_date, bmj.assignment_date , @pseudonull_date ), @pseudonull_date) as added_code_on
        , member.member_email         as email
        , member.paying_member_id     as person_id
        , code.amenities_id           as access_id
        , sum(coalesce(pur.purchased_spaces_qty,0)) as qty
    FROM
        PayingMember member
    JOIN
        BarcodeMemberJoin bmj
    ON
        member.member_email = @member_email -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        AND bmj.canceled = 0
        AND member.paying_member_id = bmj.paying_member_id

    JOIN
        AmenitiesAccessBarcode code
    ON
        bmj.barcode_string = code.barcode_string
        AND code.barcode_type = 'FULL_MEMBER_BARCODE'
    JOIN
        Purchase pur
    ON
        pur.barcode_string = bmj.barcode_string
        AND pur.canceled = 0

    GROUP BY
        -- NOTE: everything that gets 'selected' up top is ALSO involved in the GROUP BY with the
        --    important EXCEPTION of the fields from the Purchase table.
        bmj.barcode_string
        , IF ( bmj.assignment_date > @minimum_date, IF ( bmj.assignment_date < @maximum_date, bmj.assignment_date , @pseudonull_date ), @pseudonull_date)
        , member.member_email
        , member.paying_member_id
        , code.amenities_id

) member_owned_barcodes

JOIN
    AmenitiesAccessType access
ON
    access.amenities_id <> 1
    AND member_owned_barcodes.access_id = access.amenities_id
JOIN
    VehicleRegistration vreg
ON
        member_owned_barcodes.b_str = vreg.barcode_string
    AND
        (NOT (vreg.vehicle_id = 1 AND vreg.trial_period_id = 1))
    AND
        (vreg.trial_period_id = 1 OR ( NOW() < vreg.expiration_date ) )
    AND
        (NOT (vreg.canceled = 1))

LEFT JOIN
    Vehicle veh
ON
    vreg.vehicle_id = veh.vehicle_id

LEFT JOIN
    GuestTrialPeriod trial
ON
    vreg.trial_period_id = trial.trial_period_id
LEFT JOIN
    InvitedGuest guest
ON
    trial.guest_id = guest.guest_id

GROUP BY
    -- We do a group by on (barcode, vehicle, trial id) as a SANITY
    -- CHECK. Those three fields should UNIQUELY identify one
    -- VehicleRegistration.  Therefore, if we ever get MORE THAN ONE
    -- row at a time that can POSSIBLY be aggregated together under
    -- this clause, then something is really wrong (either we have
    -- corrupt data or a bad SQL query or both)
    member_owned_barcodes.b_str
    , vreg.vehicle_id
    , vreg.trial_period_id
;

