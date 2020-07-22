# khinkaV2

Simple CRUD project: H2 + doobie + IO

Represents a Restaurant, with *Clients* taking *Orders* consisting of *Items*:)

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

When the prompt shows up, type `help` to get a list of available commands
