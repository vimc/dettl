#' Prepare example db
#'
#' Create a SQLite database for testing called test.sqlite in specified
#' directory. This boostraps a simple table called people with data optionally
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
#' @param add_job_table If TRUE also boostrap job table related to people table
#' @param log_table_name The name of the log table to create in the test DB
#'
#' @keywords internal
prepare_example_db <- function(dir, add_data = FALSE, add_job_table = FALSE,
                               log_table_name = "log_table") {
  path <- file.path(dir, "test.sqlite")
  if (file.exists(path)) {
    ## Ensure we always start with a fresh DB
    file.remove(path)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  sqlite_enable_fk(con)
  people_query <- DBI::dbSendQuery(con,
    "CREATE TABLE people (
      id     INTEGER PRIMARY KEY,
      name   TEXT,
      age    INTEGER,
      height INTEGER
    )"
  )
  DBI::dbClearResult(people_query)
  if (add_data) {
    person <- data.frame(list(
      name = "Daisy",
      age = 34,
      height = 189
    ), stringsAsFactors = FALSE)

    DBI::dbWriteTable(con, "people", person, append = TRUE)
  }

  if (add_job_table) {
    job_query <- DBI::dbSendQuery(con,
      "CREATE TABLE jobs (
        id     INTEGER PRIMARY KEY,
        job    TEXT,
        person INTEGER,
        FOREIGN KEY (person) REFERENCES people(id)
      )"
    )
    DBI::dbClearResult(job_query)
  }

  log_table_query <- DBI::dbSendQuery(con, sprintf(
    "CREATE TABLE %s (
      name       TEXT,
      date       REAL,
      comment    TEXT,
      git_user   TEXT,
      git_email  TEXT,
      git_branch TEXT,
      git_hash   TEXT
    )",
    log_table_name))
  DBI::dbClearResult(log_table_query)

  DBI::dbDisconnect(con)
  path
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
#' @param add_job_table If TRUE also boostrap job table related to people table.
#'
#' @keywords internal
prepare_test_import <- function(example_dir = "example",
                                dettl_config = "dettl_config.yml",
                                create_db = TRUE,
                                add_data = FALSE, add_job_table = FALSE) {
  path <- build_git_demo(example_dir, dettl_config)
  if (create_db) {
    prepare_example_db(path, add_data, add_job_table)
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
