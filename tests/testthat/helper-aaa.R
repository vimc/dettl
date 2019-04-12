local({
  dbname <- "dettl_test_db"
  user <- "postgres"
  host <- "localhost"
  con_pg <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, user = user,
                           host = host)
  DBI::dbDisconnect(con_pg)
})
