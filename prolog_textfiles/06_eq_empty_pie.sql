

create temporary table Supplier
(
    s_id int unsigned not null,
    primary key (s_id)
);

create temporary table Part
(
    p_id int unsigned not null,
    primary key (p_id)
);

create temporary table SPJoin
(
    s_id int unsigned not null,
    p_id int unsigned not null,
    primary key (s_id,p_id)
);


SELECT * FROM Supplier s WHERE NOT EXISTS ( SELECT * FROM Part p WHERE NOT EXISTS ( SELECT * FROM SPJoin sp WHERE s.s_id = sp.s_id and sp.p_id = p.p_id ) );


insert into Supplier values (0),(1);

insert into SPJoin values (0,0),(0,1);


delete from SPJoin;
