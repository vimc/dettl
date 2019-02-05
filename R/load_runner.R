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
#'
#' @keywords internal
#'
run_load <- function(load, con, transformed_data, test_queries, path,
                     test_file) {
  if (is.null(transformed_data)) {
    stop("Cannot run tests as no data has been transformed.")
  }
  before <- test_queries(con)
  DBI::dbBegin(con)
  transaction_active <- TRUE
  on.exit(if (transaction_active) {DBI::dbRollback(con)})
  load(transformed_data, con)
  after <- test_queries(con)
  test_results <- run_load_tests(path, test_file, before, after)
  if (all_passed(test_results)) {
    DBI::dbCommit(con)
    transaction_active <- FALSE
  } else {
    DBI::dbRollback(con)
    transaction_active <- FALSE
    stop("Failed to load data - not all tests passed.")
  }
  invisible(TRUE)
}
