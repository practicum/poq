

CREATE TEMPORARY TABLE Supplier
( s_id INT UNSIGNED NOT NULL,
  PRIMARY KEY (s_id));

CREATE TEMPORARY TABLE Part
( p_id INT UNSIGNED NOT NULL,
  PRIMARY KEY (p_id));

CREATE TEMPORARY TABLE SPJoin
( s_id INT UNSIGNED NOT NULL,
  p_id INT UNSIGNED NOT NULL,
  PRIMARY KEY (s_id,p_id));


SELECT * FROM Supplier s WHERE NOT EXISTS ( SELECT * FROM Part p WHERE NOT EXISTS ( SELECT * FROM SPJoin sp WHERE s.s_id = sp.s_id AND sp.p_id = p.p_id ) );


insert into Supplier values (0),(1);

insert into SPJoin values (0,0),(0,1);


delete from SPJoin;
