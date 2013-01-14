


CREATE TEMPORARY TABLE EntityA
( int_a INT UNSIGNED UNIQUE );


CREATE TEMPORARY TABLE EntityB
( int_b INT UNSIGNED NOT NULL UNIQUE );


insert into EntityA values (2),(NULL),(0);

insert into EntityB values (2);



-- QA: result never has NULL
    SELECT * FROM EntityA a WHERE a.int_a NOT IN ( SELECT int_b FROM EntityB );

-- QB: (can have NULL in result)
    SELECT * FROM EntityA a WHERE NOT EXISTS ( SELECT int_b FROM EntityB b WHERE b.int_b = a.int_a );

