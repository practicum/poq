
-- info that will be stored in a QR code on the sticker that a camping
-- member will stick on their vehicle. park employees have a hand-held
-- tool that decodes the QR code and shows the info in human-readable
-- form.  the employee can then cross-check that the info (including
-- vehicle information, etc) actually matches the vehicle. this makes
-- the QR code stickers hard to forge. at a minimum, it means you
-- cannot simply photocopy the QR code and stick it on other vehicles.

-- for further security it could be deemed necessary to encrypt the
-- info, then generate a QR code from the encrypted data, which would
-- mean that the hand-held tools of the park employees would decode
-- the QR code and then also decrypt the info in order to display it
-- in human-readable form.


-- sample meaningful parameter combo:
-- select 'known_good_barcode' into @code;
-- select 5 into @v_id;
-- select 1 into @t_id;
-- select 4 into @m_id;


SELECT

    if( vreg.trial_period_id = 1
            , vreg.barcode_string
            , trial.barcode_string
      )
    as barcode
    , if( vreg.trial_period_id = 1
            , 'FULL_MEMBER_BARCODE'
            , 'GUEST_BARCODE'
        )
    as barcode_type
    , vreg.vehicle_id as v_id
    , vreg.trial_period_id as t_id
    , vreg.registration_date
    , vreg.expiration_date

    , access.amenities_access_level_name as a_level

    , pd.fullname as fname
    , pd.organization as org

FROM
    VehicleRegistration vreg
JOIN
    AmenitiesAccessBarcode code
ON
    vreg.barcode_string = @code -- <<<<<<<<<<<<<< FULL_MEMBER_BARCODE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    AND vreg.vehicle_id = @v_id  -- <<<<<<<<<<<<<<<<<<<<<< VEHICLE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    AND vreg.trial_period_id = @t_id  -- <<<<<<<<<<<<<<<<<<<<<<<< TRIAL PERIOD ID <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    AND vreg.barcode_string = code.barcode_string
    AND code.amenities_id <> 1
JOIN
    AmenitiesAccessType access
ON
    code.amenities_id = access.amenities_id
JOIN
    GuestTrialPeriod trial
ON
    vreg.trial_period_id = trial.trial_period_id
    AND trial.canceled = 0
JOIN
    InvitedGuest guest
ON
    trial.guest_id = guest.guest_id
JOIN
    BarcodeMemberJoin bmj
ON
    bmj.barcode_string = vreg.barcode_string
    AND bmj.paying_member_id =  @m_id -- <<<<<<<<<<<<<<< FULL MEMBER! (not guest email) <<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    AND bmj.canceled = 0
JOIN
    PayingMember member
ON
    bmj.paying_member_id = member.paying_member_id
JOIN
    PersonalData pd
ON
    member.personal_data_id = pd.personal_data_id

;


-- target assertions we would like to verify:

-- result count MUST BE 1 or 0

-- Discussion: if this query ever returns MORE THAN ONE ROW, then we
-- could potentially end up storing TOO MANY vehicle traits in the QR
-- code sticker (because of a later join not shown here).  However,
-- even in the case of "too many vehicle traits" that I just
-- mentioned... we will still be GUARANTEED to store traits ONLY of
-- the CORRECT VEHICLE. There will just be duplicates.
