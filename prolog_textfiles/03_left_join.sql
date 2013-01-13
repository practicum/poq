

create temporary table Person
(
p_id int unsigned not null,
primary key (p_id)
);

create temporary table ExtraInfo
(
p_id int unsigned not null,
title varchar(20) not null,
primary key (p_id)
);

insert into Person values (2),(0),(1);
insert into  ExtraInfo values (0,'mrs'),(1,'mr');



-- QC: (good)
    SELECT * FROM Person p LEFT JOIN ExtraInfo ei ON p.p_id = ei.p_id WHERE coalesce( ei.title, '') <> 'mr';

-- QD: (bad)
    SELECT * FROM Person p LEFT JOIN ExtraInfo ei ON p.p_id = ei.p_id and coalesce( ei.title, '') <> 'mr';


