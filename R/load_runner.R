#' Run the load step ensuring tests pass before db changes are committed.
#'
#' Runs the load function on the DB within a transaction. Then run a set of
#' tests on the DB and rollback the changes if any should fail.
#'
#' @param load The load function for making the DB changes.
#' @param con Connection to the database.
#' @param transformed_data List of data frames representing the data to be
#' loaded to the DB.
#' @param test_queries Function containing queries for running on the DB before
#' and after the load is run. Used within the tests to check changes to the DB.
#' @param path Path to the import project directory.
#' @param test_file Path to file containing the testthat tests for verifying the
#' DB changes.
#' @param dry_run If TRUE then any database changes are rolled back when the
#' import completes. i.e. the load stage can be run and tests executed but the
#' db will be rolled back.
#' @param comment An optional comment to add to the import log table for this
#' run.
#'
#' @keywords internal
#'
run_load <- function(con, load, transformed_data, test_queries, path,
                     test_file, dry_run, log_table, comment) {
  if (is.null(transformed_data)) {
    stop("Cannot run tests as no data has been transformed.")
  }
  before <- test_queries(con)
  DBI::dbBegin(con)
  transaction_active <- TRUE
  on.exit(if (transaction_active) {DBI::dbRollback(con)})
  load(transformed_data, con)
  after <- test_queries(con)
  test_path <- file.path(path, test_file)
  message(sprintf("Running load tests %s", test_path))
  test_results <- run_load_tests(test_path, before, after, con)
  if (all_passed(test_results)) {
    if (dry_run) {
      DBI::dbRollback(con)
      message("All tests passed, rolling back dry run import.")
    } else {
      message("All tests passed, commiting changes to database.")
      log_import(con, log_table, path, comment)
      DBI::dbCommit(con)
    }
    transaction_active <- FALSE
  } else {
    DBI::dbRollback(con)
    transaction_active <- FALSE
    stop("Failed to load data - not all tests passed.")
  }
  invisible(TRUE)
}
