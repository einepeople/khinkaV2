# khinkaV2

Simple CRUD project: H2 + doobie + filthy attempts to make it pure'n'functional

Made for me to graduate from the Uni

UPD: [mood](https://coub.com/view/29jdfr)

UPD2: [mood](https://www.youtube.com/watch?v=ZHrLNqDVewM)

# How to Яun

For DB population you could either:

1. Run an H2 DB server
2. Apply backups from `src/main/resources/backup.zip`

Or 

1. Run an H2 DB server
2. Create schema and table by applying `src/main/resources/tables.sql` to your DB
3. Populate DB with `data_users.sql`, `data_items.sql`, `data_orders.sql` and `data_buckets.sql` (in the given order)

After that change `src/main/resources/application.conf` to correspond to your DB instance: 
* driver 
* hostname
* login
* password

Run the application using `src/main/scala/crukhalnaya/Main`