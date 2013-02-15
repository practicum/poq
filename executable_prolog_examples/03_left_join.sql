--
-- https://github.com/practicum/poq
--
-- Executable SQL code from example 3 of Chapter 5.
--


CREATE TABLE Person
(
    p_id INT UNSIGNED NOT NULL,

    PRIMARY KEY (p_id)
);

CREATE TABLE ExtraInfo
(
    p_id  INT UNSIGNED NOT NULL,
    title VARCHAR(20) NOT NULL,

    PRIMARY KEY (p_id)
);

INSERT INTO Person VALUES (2),(0),(1);

INSERT INTO ExtraInfo VALUES (0,'mrs'),(1,'mr');



-- QC: (good)
    SELECT * FROM Person p
    LEFT JOIN ExtraInfo ei ON p.p_id = ei.p_id
    WHERE coalesce( ei.title, '') <> 'mr';

-- QD: (bad)
    SELECT * FROM Person p
    LEFT JOIN ExtraInfo ei ON p.p_id = ei.p_id
        AND coalesce( ei.title, '') <> 'mr';