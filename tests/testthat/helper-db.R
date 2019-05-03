## Create a simple "people" table in the psotgres DB for testing.
prepare_example_postgres_db <- function() {
  dbname <- "dettl_test_db"
  user <- "postgres"
  host <- "localhost"
  dettl_test_postgres_connection(dbname, user, host)
  con <- get_postgres_connection(dbname, user, host)
  ## Make sure we have a fresh "people" table if one existed already
  DBI::dbExecute(con, "DROP TABLE IF EXISTS people")

  DBI::dbExecute(con,
    "CREATE TABLE people (
      id     BIGSERIAL PRIMARY KEY,
      name   TEXT,
      age    INTEGER,
      height INTEGER
    )"
  )

  ## Make sure we have a fresh "dettl_import_log" table if one existed already
  drop_log <- DBI::dbExecute(con,
    "DROP TABLE IF EXISTS dettl_import_log")
  query_text <- read_lines(
    system.file("sql", "postgresql", "create_log_table.sql", package = "dettl"))
  DBI::dbExecute(con, query_text)
  con
}

dettl_test_postgres_connection <- function(dbname, user, host) {
  tryCatch(
    get_postgres_connection(dbname, user, host),
    error = function(e) testthat::skip(sprintf(
    "Failed to open db connection to postgres db %s with user %s and host %s.",
    dbname, user, host))
  )
}

get_postgres_connection <- function(dbname, user, host) {
  DBI::dbConnect(RPostgres::Postgres(), dbname = dbname, user = user,
                 host = host)
}

dbi_db_connect <- function(drv, ...) {
  con <- DBI::dbConnect(drv, ...)
  sqlite_enable_fk(con)
  con
}

trigger_dbi_warning <- function() {
  oo <- options(warn = 0)
  on.exit(options(oo))
  con <- dbi_db_connect(RSQLite::SQLite(), ":memory:")
  rm(con)
  suppressWarnings(gc())
}

trigger_dbi_warning()

get_local_connection <- function() {
  dbi_db_connect(RSQLite::SQLite(), ":memory:")
}

