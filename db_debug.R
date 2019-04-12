con_sqlite <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
DBI::dbDisconnect(con_sqlite)

dbname <- "dettl_test_db"
user <- "postgres"
host <- "localhost"
con_pg <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, user = user,
                         host = host)
DBI::dbDisconnect(con_pg)
