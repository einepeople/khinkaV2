create schema if not exists KHINKA;

create table "Users"
(
	ID INT auto_increment,
	NAME VARCHAR(255) not null,
	ADDRESS VARCHAR(255) not null,
	BIRTHDATE DATE not null,
	SEX BOOL not null,
	constraint USERS_PK
		primary key (ID)
);

create table "Items"
(
	ID INT auto_increment,
	NAME VARCHAR(255) not null,
	DESCRIPTION VARCHAR(255) not null,
	PRICE DOUBLE not null,
	AMOUNT INT not null,
	constraint ITEMS_PK
		primary key (ID)
);

create table "Orders"
(
	ID INT auto_increment,
	PLACED DATETIME not null,
	CLIENT INT not null,
	DELIVERTO VARCHAR(511) not null,
	constraint ORDERS_PK
		primary key (ID),
	constraint ORDERS_USERS_ID_FK
		foreign key (CLIENT) references "Users" (ID)
			on delete cascade
);

create table "Buckets"
(
	ID INT not null,
	ITEM INT not null,
	AMOUNT INT not null,
	constraint BUCKETS_PK
		primary key (ID, ITEM),
	constraint BUCKETS_ORDERS_ID_FK
		foreign key (ID) references "Orders" (ID)
			on delete cascade,
	constraint BUCKET_TO_ITEM
		foreign key (ITEM) references "Items" (ID)
			on delete cascade
);

