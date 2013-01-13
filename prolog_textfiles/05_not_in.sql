


create temporary table EntityA
(
    int_a int unsigned unique
);


create temporary table EntityB
(
    int_b int unsigned not null unique
);


insert into EntityA values (2),(NULL),(0);

insert into EntityB values (2);



-- QA: result never has NULL
    SELECT * FROM EntityA a WHERE a.int_a NOT IN ( SELECT int_b FROM EntityB );

-- QB: (can have NULL in result)
    SELECT * FROM EntityA a WHERE NOT EXISTS ( SELECT int_b FROM EntityB b WHERE b.int_b = a.int_a );