--
-- https://github.com/practicum/poq
--
-- Executable SQL code from example 2 of Chapter 5.
--


CREATE TABLE ShoppingCart
(
    cart      VARCHAR(20),
    cart_date DATE NOT NULL,

    PRIMARY KEY (cart)
);

CREATE TABLE CartDetail
(
    cart    VARCHAR(20),
    product VARCHAR(20),
    PRIMARY KEY (cart,product)
);


SELECT max( c.cart ) AS recent_cart_c,
       max( c.cart_date ) AS recent_date,
       max( ci.cart ) AS recent_cart_ci,
       ci.product
FROM ShoppingCart c JOIN CartDetail ci
ON c.cart = ci.cart
GROUP BY ci.product;



INSERT INTO ShoppingCart VALUES 
    ( 'fccy463', from_unixtime(86400*(1+0)) );

INSERT INTO ShoppingCart VALUES 
    ( 'ddd213',  from_unixtime(86400*(1+1)) );

INSERT INTO CartDetail VALUES ('ddd213','aspirin');

INSERT INTO CartDetail VALUES ('fccy463','aspirin');
