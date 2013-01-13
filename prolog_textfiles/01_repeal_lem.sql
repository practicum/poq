

-- 'problematic' definition where middle initial can be null
create temporary table Person
(
first varchar(20) not null,
middle varchar(20) null,
last varchar(20) not null
);

-- 'repaired' definition where middle initial can be null
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
