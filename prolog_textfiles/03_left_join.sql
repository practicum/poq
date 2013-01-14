

CREATE TEMPORARY TABLE Person
( p_id INT UNSIGNED NOT NULL,
  PRIMARY KEY (p_id));

CREATE TEMPORARY TABLE ExtraInfo
( p_id  INT UNSIGNED NOT NULL,
  title VARCHAR(20) NOT NULL,
  PRIMARY KEY (p_id));

insert into Person values (2),(0),(1);
insert into  ExtraInfo values (0,'mrs'),(1,'mr');



-- QC: (good)
    SELECT * FROM Person p LEFT JOIN ExtraInfo ei ON p.p_id = ei.p_id WHERE coalesce( ei.title, '') <> 'mr';

-- QD: (bad)
    SELECT * FROM Person p LEFT JOIN ExtraInfo ei ON p.p_id = ei.p_id AND coalesce( ei.title, '') <> 'mr';


