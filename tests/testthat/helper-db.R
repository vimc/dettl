## Create a simple "people" table in the psotgres DB for testing.
prepare_example_postgres_db <- function(create_log = TRUE, add_fk_data = FALSE) {
  dbname <- "dettl_test_db"
  user <- "postgres"
  host <- "localhost"
  dettl_test_postgres_connection(dbname, user, host)
  con <- get_postgres_connection(dbname, user, host)
  ## Make sure we have a fresh "people" table if one existed already
  DBI::dbExecute(con, "DROP TABLE IF EXISTS people CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS region CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS street CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS address CASCADE")

  DBI::dbExecute(con,
    "CREATE TABLE people (
      id     BIGSERIAL PRIMARY KEY,
      name   TEXT,
      age    INTEGER,
      height INTEGER
    )"
  )

  if (add_fk_data) {
    DBI::dbExecute(con,
      "CREATE TABLE region (
        name TEXT PRIMARY KEY,
        parent TEXT,
        FOREIGN KEY (parent) REFERENCES region(name)
      )")
    region1 <- data.frame(list(
      name = "UK"
    ))
    region2 <- data.frame(list(
      name = "London",
      parent = "UK"
    ))
    DBI::dbWriteTable(con, "region", region1, append = TRUE)
    DBI::dbWriteTable(con, "region", region2, append = TRUE)

    DBI::dbExecute(con,
      "CREATE TABLE street (
        name TEXT PRIMARY KEY
      )")
    street1 <- data.frame(list(
      name = "Commercial Road"
    ))
    street2 <- data.frame(list(
      name = "The Street"
    ))
    DBI::dbWriteTable(con, "street", street1, append = TRUE)
    DBI::dbWriteTable(con, "street", street2, append = TRUE)

    DBI::dbExecute(con,
      "CREATE TABLE address (
        street TEXT,
        region TEXT,
        FOREIGN KEY (street) REFERENCES street(name),
        FOREIGN KEY (region) REFERENCES region(name)
      )")
    address <- data.frame(list(
      street = "The Street",
      region = "London"
    ), stringsAsFactors = FALSE)
    DBI::dbWriteTable(con, "address", address, append = TRUE)
  }

  ## Make sure we have a fresh "dettl_import_log" table if one existed already
  drop_log <- DBI::dbExecute(con,
    "DROP TABLE IF EXISTS dettl_import_log")
  if (create_log) {
    query_text <- read_lines(
      system.file("sql", "postgresql", "create_log_table.sql", package = "dettl"))
    DBI::dbExecute(con, query_text)
  }
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

