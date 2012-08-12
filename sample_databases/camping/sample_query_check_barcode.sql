
-- given a barcode, we use this query to find out whether it is a
-- full-membership barcode or a trial membership barcode, and we also
-- find out which amenities access level is associated with this
-- barcode and (in the full-membership case) how many purchase
-- transactions took place for this barcode.  We might also discover
-- that the barcode is not known in the system at all.

SELECT
      cnt
    , a_id
    , a_id_check
    , bc_type
    , bc_type_check
    , access.amenities_access_level_name
FROM
    (

        SELECT
              count(amenities_id)                as cnt
            , coalesce( min(amenities_id), 1 )   as a_id
            , coalesce( max(amenities_id), 1 )   as a_id_check
            , coalesce( min(barcode_type), 'x' ) as bc_type
            , coalesce( max(barcode_type), 'x' ) as bc_type_check
        FROM
            AmenitiesAccessBarcode code
        JOIN
            Purchase pur
        ON
                code.barcode_string = @barcode   -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        AND
                code.barcode_string = pur.barcode_string
        AND
                coalesce(pur.canceled,0) = 0

    UNION ALL

        SELECT
              count(amenities_id)                 as cnt
            , coalesce( min(amenities_id), 1 )    as a_id
            , coalesce( max(amenities_id), 1 )    as a_id_check
            , coalesce( min(barcode_type), 'x' )  as bc_type
            , coalesce( max(barcode_type), 'x' )  as bc_type_check
        FROM
            AmenitiesAccessBarcode code
        JOIN
            GuestTrialPeriod trial
        ON
                code.barcode_string = @barcode   -- <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        AND
                code.barcode_string = trial.barcode_string
        AND
                coalesce(trial.canceled,0) = 0

    ) unified

LEFT JOIN
    AmenitiesAccessType access
ON
    coalesce( unified.a_id , 1 ) = access.amenities_id

ORDER BY cnt DESC;



--  TO BE TESTED: query always returns exactly two rows.
--  TO BE TESTED: second row always has cnt = 0. (should always be the 2nd row due to the ORDER BY)
--  TO BE TESTED: any row with zero should have 'x'
--  TO BE TESTED: min and max should match on a per-row basis