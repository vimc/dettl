#' Prepare example db
#'
#' Create a SQLite database for testing called test.sqlite in specified
#' directory. This bootstraps a simple table called people with data optionally
#' added. This should only be called from a test, vignette or roxygen example.
#'
#' Create a simple "people" table in the DB for testing the import process.
#' Ideally this would be in a helper-*.R file in testthat directory but we
#' need to be able to setup a DB for vignette.
#' Expect the calling function to cleanup the created db if it is not in a
#' tempdir.
#'
#' @param dir Directory to create the db in
#' @param add_data If TRUE data is bootstrapped to people table
#' @param add_job_table If TRUE also bootstrap job table related to people table
#' @param add_log_table If TRUE also bootstrap log table using sql in inst dir
#'
#' @keywords internal
prepare_example_db <- function(dir, add_data = FALSE, add_job_table = FALSE,
                               add_log_table = TRUE, add_fk_data = FALSE,
                               add_cyclic_fks = FALSE) {
  path <- file.path(dir, "test.sqlite")
  if (file.exists(path)) {
    ## Ensure we always start with a fresh DB
    file.remove(path)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  sqlite_enable_fk(con)
  DBI::dbExecute(con,
    "CREATE TABLE people (
      id     INTEGER PRIMARY KEY NOT NULL,
      name   TEXT NOT NULL,
      age    INTEGER NOT NULL,
      height INTEGER
    )"
  )
  if (add_data) {
    person <- data_frame(
      name = "Daisy",
      age = 34,
      height = 189)

    DBI::dbWriteTable(con, "people", person, append = TRUE)
  }

  if (add_job_table) {
    add_job_table(con)
  }

  if (add_log_table) {
    query_text <- read_lines(
      dettl_file("sql", "postgresql", "create_log_table.sql"))
    DBI::dbExecute(con, query_text)
  }

  if (add_fk_data) {
    add_fk_data(con)
  }

  if (add_cyclic_fks) {
    add_cyclic_fk_tables(con)
  }

  DBI::dbDisconnect(con)
  path
}

add_job_table <- function(con) {
  dialect <- sql_dialect(con)
  switch(
    dialect,
    "sqlite" = DBI::dbExecute(con,
      "CREATE TABLE jobs (
      id     INTEGER PRIMARY KEY NOT NULL,
      job    TEXT,
      person INTEGER,
      FOREIGN KEY (person) REFERENCES people(id)
      )"
    ),
    "postgresql" = DBI::dbExecute(con,
      "CREATE TABLE jobs (
      id     SERIAL UNIQUE NOT NULL,
      job    TEXT,
      person INTEGER,
      FOREIGN KEY (person) REFERENCES people(id)
      )"
    )
  )
}

#' Add tables with foreign key constraints for testing
#'
#' Create some tables with foreign key constraints and add some simple data
#' to them for testing.
#'
#' @param con Connection to DB to add tables with foreign key constraints
#'
#' @keywords internal
add_fk_data <- function(con) {
  dialect <- sql_dialect(con)
  get_fks <- switch(
    dialect,
    "sqlite" = DBI::dbExecute(con,
      "CREATE TABLE region (
      id INTEGER PRIMARY KEY,
      name TEXT,
      parent INTEGER,
      FOREIGN KEY (parent) REFERENCES region(id)
    )"),
    "postgresql" = DBI::dbExecute(con,
        "CREATE TABLE region (
         id SERIAL UNIQUE,
         name TEXT,
         parent INTEGER,
         FOREIGN KEY (parent) REFERENCES region(id)
      )"))
  region1 <- data.frame(
    name = "UK"
  )
  region2 <- data.frame(
    name = "London",
    parent = 1
  )
  DBI::dbWriteTable(con, "region", region1, append = TRUE)
  DBI::dbWriteTable(con, "region", region2, append = TRUE)
  DBI::dbExecute(con,
    "CREATE TABLE street (
      name TEXT PRIMARY KEY
    )")
  street1 <- data.frame(
    name = "Commercial Road"
  )
  street2 <- data.frame(
    name = "The Street"
  )
  DBI::dbWriteTable(con, "street", street1, append = TRUE)
  DBI::dbWriteTable(con, "street", street2, append = TRUE)
  DBI::dbExecute(con,
    "CREATE TABLE address (
      street TEXT,
      region INTEGER,
      FOREIGN KEY (street) REFERENCES street(name),
      FOREIGN KEY (region) REFERENCES region(id)
    )")
  address <- data_frame(
    street = "The Street",
    region = 2)
  DBI::dbWriteTable(con, "address", address, append = TRUE)
}

add_cyclic_fk_tables <- function(con) {
  dialect <- sql_dialect(con)
  switch(
    dialect,
    "sqlite" = {
      DBI::dbExecute(con,
        "CREATE TABLE model (
        id TEXT PRIMARY KEY,
        current_version INTEGER,
        FOREIGN KEY (current_version) REFERENCES model_version(id)
      )")
      DBI::dbExecute(con,
        "CREATE TABLE model_version (
        id INTEGER PRIMARY KEY,
        model TEXT,
        FOREIGN KEY (model) REFERENCES model(id)
      )")
      },
    "postgresql" = {
      DBI::dbExecute(con,
        "CREATE TABLE model (
        id TEXT PRIMARY KEY,
        current_version INTEGER
      )")
      DBI::dbExecute(con,
        "CREATE TABLE model_version (
        id SERIAL UNIQUE,
        model TEXT,
        FOREIGN KEY (model) REFERENCES model(id)
      )")
      DBI::dbExecute(con,
        "ALTER TABLE model ADD CONSTRAINT fk_model_version
        FOREIGN KEY (current_version) REFERENCES model_version (id)")
    }
  )
}

#' Prepare example import inside a git repo
#'
#' Copies an example import to a new temp directory, sets up git for the
#' directory and creates a test SQLite DB in the temp directory as test.sqlite.
#'
#' This should only be called from a test, vignette or roxygen example.
#'
#' @param example_dir The example directory to copy to temp.
#' @param dettl_config Path to the dettl config file.
#' @param create_db If TRUE then test SQLite db will be created
#' @param add_data If TRUE data is bootstrapped to people table in test DB.
#' @param add_job_table If TRUE also bootstrap job table related to people table.
#' @param add_log_table If TRUE then also bootstrap log table.
#' @param add_fk_data If TRUE then bootstrap three tables with foreign key
#' @param add_cyclic_fks If TRUE then bootstrap two tables with cyclic foreign
#' key constraints.
#' constraints for testing automatic reading of foreign key constraints from db.
#'
#' @examples
#' dettl:::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
prepare_test_import <- function(example_dir = "example",
                                dettl_config = "dettl_config.yml",
                                create_db = TRUE,
                                add_data = FALSE, add_job_table = FALSE,
                                add_log_table = TRUE,
                                add_fk_data = FALSE,
                                add_cyclic_fks = FALSE) {
  path <- build_git_demo(example_dir, dettl_config)
  if (create_db) {
    prepare_example_db(path, add_data, add_job_table, add_log_table,
                       add_fk_data, add_cyclic_fks)
  }
  path
}


#' Setup dettl import in a tempdir.
#'
#' @param dir The example directory to copy to tempdir.
#'
#' @keywords internal
setup_dettl <- function(dir, dettl_config) {
  path <- temp_file()
  dir.create(path)
  file.copy(dir, path, recursive = TRUE)
  file.copy(dettl_config, path)
  path
}
