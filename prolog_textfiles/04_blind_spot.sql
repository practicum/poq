

CREATE TEMPORARY TABLE Employee
( dept   INT UNSIGNED NOT NULL,
  emp    VARCHAR(20)  NOT NULL,
  salary INT UNSIGNED NOT NULL,
  PRIMARY KEY (emp));



-- Query A: (good)
SELECT dept,
  count(emp),
  sum( case when salary < 1000
            then 1
            else 0 end ) as low_wage
FROM Employee GROUP BY dept;

-- Query B: (flawed)
SELECT dept,
  count(emp),
  count(salary) as low_wage_count
FROM Employee WHERE salary < 1000
GROUP BY dept;



insert into Employee values (0,'jacob',850);

insert into Employee values (0,'william',850);

insert into Employee values (1,'isabella',4500);

