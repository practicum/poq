--
-- https://github.com/practicum/poq
--
-- Executable SQL code from example 1 of Chapter 5.
--


-- problematic definition where middle initial can be NULL
CREATE TABLE Person
(
    first  VARCHAR(20) NOT NULL,
    middle VARCHAR(20) NULL,
    last   VARCHAR(20) NOT NULL
);

-- repaired definition where middle initial cannot be null
CREATE TABLE Person
(
    first  VARCHAR(20) NOT NULL,
    middle VARCHAR(20) NOT NULL,
    last   VARCHAR(20) NOT NULL
);



SELECT * FROM Person WHERE middle = 'P'
UNION
SELECT * FROM Person WHERE NOT middle = 'P';


INSERT INTO Person VALUES ('william','jacob', 'isabella');

INSERT INTO Person VALUES ('jacob',NULL,'jacob');
