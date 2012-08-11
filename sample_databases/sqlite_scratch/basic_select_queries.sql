
-- return entire column from single table
select email_address from CustomerAccount;

-- join two tables on a single equality condition
select ca.email_address From CustomerAccount ca join AccountDescription ad on ca.account_desc_id = ad.account_desc_id;

-- danger! select where a field is EQUAL to a subselect. subselect could be an empty set
select p.purchase_id from Purchase p where p.licnum = ( select licnum from Licnum l where l.product_id = 1 );

-- danger! select where a field is EQUAL to a subselect. subselect could have cardinality GREATER THAN ONE.
select p.purchase_id from Purchase p where p.licnum = ( select licnum from Licnum l where l.product_id = 2 );

select p.purchase_id from Purchase p where p.licnum in ( select licnum from Licnum l where l.product_id = 2 );

-- Error: only a single result allowed for a SELECT that is part of an expression
-- select p.purchase_id from Purchase p where p.licnum = ( select licnum, l.product_id from Licnum l where l.product_id = 2 );

-- this will return the numeric value 0
select count(*) from IntentionallyEmptyTable;

-- this will return NULL
select sum(1) from IntentionallyEmptyTable;

-- this will only count rows where NON-NULL values of 'col_1' are found
select count(col_1) from IntentionallyNullableTable;

-- this will count all rows
select count(1) from IntentionallyNullableTable;



