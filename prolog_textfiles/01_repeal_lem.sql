

-- 'problematic' definition where middle initial can be null
CREATE TEMPORARY TABLE Person
(
    first  VARCHAR(20) NOT NULL,
    middle VARCHAR(20) NULL,
    last   VARCHAR(20) NOT NULL
);

-- 'repaired' definition where middle initial cannot be null
create temporary table Person
(
first varchar(20) not null,
middle varchar(20) not null,
last varchar(20) not null
);



SELECT * FROM Person WHERE middle = 'P'
UNION
SELECT * FROM Person WHERE NOT middle = 'P';


INSERT INTO Person VALUES ('william','jacob', 'isabella');

INSERT INTO Person VALUES ('jacob',NULL,'jacob');
