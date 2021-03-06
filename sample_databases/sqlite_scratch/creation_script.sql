PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE CustomerAccount
(
account_id INTEGER PRIMARY KEY AUTOINCREMENT,
email_address INTEGER NOT NULL,
password INTEGER NOT NULL,
account_desc_id INTEGER NOT NULL
);
INSERT INTO "CustomerAccount" VALUES(1,30,1111,1);
INSERT INTO "CustomerAccount" VALUES(2,31,2222,1);
INSERT INTO "CustomerAccount" VALUES(3,32,3333,3);
INSERT INTO "CustomerAccount" VALUES(4,33,4444,3);
INSERT INTO "CustomerAccount" VALUES(5,34,1111,3);
CREATE TABLE LicnumAccountJoin
(
licnum INTEGER NOT NULL,
cust_account_id INTEGER NOT NULL,
canceled INTEGER NOT NULL DEFAULT 0
);
INSERT INTO "LicnumAccountJoin" VALUES(735452,1,0);
INSERT INTO "LicnumAccountJoin" VALUES(968689,2,0);
INSERT INTO "LicnumAccountJoin" VALUES(1918171,3,0);
INSERT INTO "LicnumAccountJoin" VALUES(432335,4,0);
INSERT INTO "LicnumAccountJoin" VALUES(968689,4,0);
CREATE TABLE Licnum
(
licnum INTEGER PRIMARY KEY,
product_id INTEGER NOT NULL
);
INSERT INTO "Licnum" VALUES(432335,2);
INSERT INTO "Licnum" VALUES(635454,2);
INSERT INTO "Licnum" VALUES(687989,2);
INSERT INTO "Licnum" VALUES(735452,2);
INSERT INTO "Licnum" VALUES(968689,2);
INSERT INTO "Licnum" VALUES(1918171,2);
INSERT INTO "Licnum" VALUES(3830045,2);
INSERT INTO "Licnum" VALUES(6354545,2);
INSERT INTO "Licnum" VALUES(6453490,2);
INSERT INTO "Licnum" VALUES(6482655,2);
INSERT INTO "Licnum" VALUES(9376563,2);
CREATE TABLE Purchase
(
purchase_id INTEGER PRIMARY KEY AUTOINCREMENT,
licnum INTEGER NOT NULL,
purchase_date INTEGER NOT NULL,
quantity INTEGER NOT NULL,
canceled INTEGER NOT NULL DEFAULT 0
);
INSERT INTO "Purchase" VALUES(1,735452,50,10,0);
INSERT INTO "Purchase" VALUES(2,968689,53,1,0);
INSERT INTO "Purchase" VALUES(3,1918171,55,5,0);
INSERT INTO "Purchase" VALUES(4,432335,40,25,0);
CREATE TABLE AccountDescription
(
account_desc_id INTEGER PRIMARY KEY AUTOINCREMENT,
organization_name INTEGER
);
INSERT INTO "AccountDescription" VALUES(1,1234);
INSERT INTO "AccountDescription" VALUES(2,5678);
INSERT INTO "AccountDescription" VALUES(3,7531);
CREATE TABLE IntentionallyEmptyTable
(
id INTEGER PRIMARY KEY AUTOINCREMENT,
col_1 INTEGER NOT NULL,
col_2 INTEGER NOT NULL
);
CREATE TABLE IntentionallyNullableTable
(
id INTEGER PRIMARY KEY AUTOINCREMENT,
col_1 INTEGER,
col_2 INTEGER
);
INSERT INTO "IntentionallyNullableTable" VALUES(1,3,5);
INSERT INTO "IntentionallyNullableTable" VALUES(2,8,5);
INSERT INTO "IntentionallyNullableTable" VALUES(3,9,2);
INSERT INTO "IntentionallyNullableTable" VALUES(4,3,NULL);
INSERT INTO "IntentionallyNullableTable" VALUES(5,8,NULL);
INSERT INTO "IntentionallyNullableTable" VALUES(6,9,NULL);
INSERT INTO "IntentionallyNullableTable" VALUES(7,NULL,5);
INSERT INTO "IntentionallyNullableTable" VALUES(8,NULL,5);
INSERT INTO "IntentionallyNullableTable" VALUES(9,NULL,2);
DELETE FROM sqlite_sequence;
INSERT INTO "sqlite_sequence" VALUES('AccountDescription',3);
INSERT INTO "sqlite_sequence" VALUES('CustomerAccount',5);
INSERT INTO "sqlite_sequence" VALUES('Purchase',4);
INSERT INTO "sqlite_sequence" VALUES('IntentionallyNullableTable',9);
COMMIT;
