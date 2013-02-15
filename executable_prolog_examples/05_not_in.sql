--
-- https://github.com/practicum/poq
--
-- Executable SQL code from example 5 of Chapter 5.
--

CREATE TABLE EntityA
(
    int_a INT UNSIGNED UNIQUE
);


CREATE TABLE EntityB
(
    int_b INT UNSIGNED NOT NULL UNIQUE
);


INSERT INTO EntityA VALUES (2),(NULL),(0);

INSERT INTO EntityB VALUES (2);



-- QA: result never has NULL
    SELECT * FROM EntityA a WHERE a.int_a NOT IN
        ( SELECT int_b FROM EntityB );

-- QB: (can have NULL in result)
    SELECT * FROM EntityA a WHERE NOT EXISTS
        ( SELECT int_b FROM EntityB b
          WHERE b.int_b = a.int_a );