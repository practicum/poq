

create temporary table Employee
(
    dept int unsigned not null,
    emp varchar(20) not null,
    salary int unsigned not null,
    primary key (emp)
);



-- Query A: (good)
    SELECT dept, sum( case when salary < 1000 then 1 else 0 end ) as low_wage FROM Employee GROUP BY dept;

-- Query B: (flawed)
    SELECT dept, count(1) as low_wage FROM Employee            WHERE salary < 1000 GROUP BY dept;



insert into Employee values (0,'jacob',850);

insert into Employee values (0,'william',850);

insert into Employee values (1,'isabella',4500);
