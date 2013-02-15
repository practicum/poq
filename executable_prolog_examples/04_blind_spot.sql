--
-- https://github.com/practicum/poq
--
-- Executable SQL code from example 4 of Chapter 5.
--


CREATE TABLE Employee
(
    dept   INT UNSIGNED NOT NULL,
    emp    VARCHAR(20)  NOT NULL,
    salary INT UNSIGNED NOT NULL,

    PRIMARY KEY (emp)
);


-- Query A: (good)
SELECT dept,
  COUNT(emp),
  SUM( CASE WHEN salary < 1000
            THEN 1
            ELSE 0 END ) AS low_wage
FROM Employee GROUP BY dept;

-- Query B: (flawed)
SELECT dept,
  COUNT(emp),
  COUNT(salary) AS low_wage_count
FROM Employee WHERE salary < 1000
GROUP BY dept;



INSERT INTO Employee VALUES (0,'jacob',850);

INSERT INTO Employee VALUES (0,'william',850);

INSERT INTO Employee VALUES (1,'isabella',4500);

