## Create a simple "people" table in the psotgres DB for testing.
prepare_example_postgres_db <- function(create_log = TRUE, add_fk_data = FALSE,
                                        add_job_table = FALSE,
                                        add_multi_ref_fks = FALSE,
                                        add_cyclic_fks = FALSE) {
  dbname <- "dettl_test_db"
  user <- "postgres"
  host <- "localhost"
  dettl_test_postgres_connection(dbname, user, host)
  con <- get_postgres_connection(dbname, user, host)
  ## Make sure we have a fresh "people" table if one existed already
  DBI::dbExecute(con, "DROP TABLE IF EXISTS people CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS jobs CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS region CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS street CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS address CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS referenced_table CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS id_constraint CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS nid_constraint CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS model CASCADE")
  DBI::dbExecute(con, "DROP TABLE IF EXISTS model_version CASCADE")

  DBI::dbExecute(con,
    "CREATE TABLE people (
      id     BIGSERIAL PRIMARY KEY,
      name   TEXT NOT NULL,
      age    INTEGER NOT NULL,
      height INTEGER
    )"
  )

  if (add_job_table) {
    add_job_table(con)
  }

  if (add_fk_data) {
    add_fk_data(con)
  }

  if (add_multi_ref_fks) {
    add_postgres_multiple_referenced_fks(con)
  }

  if (add_cyclic_fks) {
    add_cyclic_fk_tables(con)
  }

  ## Make sure we have a fresh "dettl_import_log" table if one existed already
  drop_log <- DBI::dbExecute(con,
    "DROP TABLE IF EXISTS dettl_import_log")
  if (create_log) {
    query_text <- read_lines(
      dettl_file("sql", "postgresql", "create_log_table.sql"))
    DBI::dbExecute(con, query_text)
  }
  con
}

## Adds tables with two foreign key constraints. Both of which refer to
## different columns within the same table. This is only usable for postgres.
## SQLite cannot have two autoincrementing columns within one table.
add_postgres_multiple_referenced_fks <- function(con) {
  DBI::dbExecute(con,
    "CREATE TABLE referenced_table (
     id SERIAL PRIMARY KEY,
     nid SERIAL UNIQUE
     )")

  DBI::dbExecute(con,
    "CREATE TABLE id_constraint (
     name TEXT PRIMARY KEY,
     ref INTEGER,
     FOREIGN KEY (ref) REFERENCES referenced_table(id)
    )")

  DBI::dbExecute(con,
    "CREATE TABLE nid_constraint (
     name TEXT PRIMARY KEY,
     ref INTEGER,
     FOREIGN KEY (ref) REFERENCES referenced_table(nid)
     )")
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


