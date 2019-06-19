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
                               add_log_table = TRUE, add_fk_data = FALSE) {
  path <- file.path(dir, "test.sqlite")
  if (file.exists(path)) {
    ## Ensure we always start with a fresh DB
    file.remove(path)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  sqlite_enable_fk(con)
  DBI::dbExecute(con,
    "CREATE TABLE people (
      id     INTEGER PRIMARY KEY,
      name   TEXT,
      age    INTEGER,
      height INTEGER
    )"
  )
  if (add_data) {
    person <- data.frame(list(
      name = "Daisy",
      age = 34,
      height = 189
    ), stringsAsFactors = FALSE)

    DBI::dbWriteTable(con, "people", person, append = TRUE)
  }

  if (add_job_table) {
    DBI::dbExecute(con,
      "CREATE TABLE jobs (
        id     INTEGER PRIMARY KEY,
        job    TEXT,
        person INTEGER,
        FOREIGN KEY (person) REFERENCES people(id)
      )"
    )
  }

  if (add_log_table) {
    query_text <- read_lines(
      dettl_file("sql", "postgresql", "create_log_table.sql"))
    DBI::dbExecute(con, query_text)
  }

  if (add_fk_data) {
    add_fk_data(con)
  }

  DBI::dbDisconnect(con)
  path
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

#' Prepare example import inside a git repo
#'
#' Copies an example import to a new temp directory, sets up git for the
#' directory and creates a test SQLite DB in the temp directory as test.sqlite.
#'
#' This should only be called from a test, vignette or roxygen example.
#'
#' @param example_dir The example directory to copy to temp.
#' @param dettl_config Path to the dettl config file.
#' @param add_data If TRUE data is bootstrapped to people table in test DB.
#' @param add_job_table If TRUE also bootstrap job table related to people table.
#' @param add_log_table If TRUE then also bootstrap log table.
#'
#' @keywords internal
prepare_test_import <- function(example_dir = "example",
                                dettl_config = "dettl_config.yml",
                                create_db = TRUE,
                                add_data = FALSE, add_job_table = FALSE,
                                add_log_table = TRUE,
                                add_fk_data = FALSE) {
  path <- build_git_demo(example_dir, dettl_config)
  if (create_db) {
    prepare_example_db(path, add_data, add_job_table, add_log_table,
                       add_fk_data)
  }
  path
}


#' Setup dettl import in a tempdir.
#'
#' @param dir The example directory to copy to tempdir.
#'
#' @keywords internal
setup_dettl <- function(dir, dettl_config) {
  path <- tempfile()
  dir.create(path)
  file.copy(dir, path, recursive = TRUE)
  file.copy(dettl_config, path)
  path
}
