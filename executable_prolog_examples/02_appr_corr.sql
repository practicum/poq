

CREATE TEMPORARY TABLE ShoppingCart
(cart      VARCHAR(20),
 cart_date DATE NOT NULL,
 PRIMARY KEY (cart));

CREATE TEMPORARY TABLE CartDetail
(cart    VARCHAR(20),
 product VARCHAR(20),
 PRIMARY KEY (cart,product));


SELECT max( c.cart ) AS recent_cart_c,
       max( c.cart_date ) AS recent_date,
       max( ci.cart ) AS recent_cart_ci,
       ci.product
FROM ShoppingCart c JOIN CartDetail ci
ON c.cart = ci.cart
GROUP BY ci.product;



insert into ShoppingCart values ( 'fccy463', from_unixtime(86400*(1+0)) );
insert into ShoppingCart values ( 'ddd213', from_unixtime(86400*(1+1)) );


insert into CartDetail values ('ddd213','aspirin');

insert into CartDetail values ('fccy463','aspirin');

-- query result:
/*
+-------------+---------+-------------+
| recent_date | product | recent_cart |
+-------------+---------+-------------+
| 1970-01-02  | aspirin | fccy463     |
+-------------+---------+-------------+
*/

