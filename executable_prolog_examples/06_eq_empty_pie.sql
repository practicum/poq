--
-- https://github.com/practicum/poq
--
-- Executable SQL code from example 6 of Chapter 5.
--


CREATE TABLE Supplier
(
    s_id INT UNSIGNED NOT NULL,

    PRIMARY KEY (s_id)
);

CREATE TABLE Part
(
    p_id INT UNSIGNED NOT NULL,

    PRIMARY KEY (p_id)
);

CREATE TABLE SPJoin
(
    s_id INT UNSIGNED NOT NULL,
    p_id INT UNSIGNED NOT NULL,

    PRIMARY KEY (s_id,p_id)
);


SELECT * FROM Supplier s WHERE NOT EXISTS
  ( SELECT * FROM Part p WHERE NOT EXISTS
    ( SELECT * FROM SPJoin sp WHERE s.s_id = sp.s_id
      AND sp.p_id = p.p_id ) );


INSERT INTO Supplier VALUES (0),(1);

INSERT INTO SPJoin VALUES (0,0),(0,1);

DELETE FROM SPJoin;
