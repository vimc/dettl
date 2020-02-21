## This is necessary because if the tests run using SQLite driver first then
## trying to connect to a postgres DB fails. Not clear why this is the case,
## indeed it looks like the error reason is coming from SQLite code even though
## trying to connect using the postgres driver.
## Forcing connect to postgres first solves this so fix like this for now.

local({
  dbname <- "dettl_test_db"
  user <- "postgres"
  host <- "localhost"
  tryCatch({
    con_pg <- DBI::dbConnect(RPostgres::Postgres(), dbname = dbname,
                             user = user, host = host)
    DBI::dbDisconnect(con_pg)
  }, error = function(e) {
    ## Just consume the error, we expect that this may not exist when being run
    ## locally
  })
})
