#' Run the load step ensuring tests pass before db changes are committed.
#'
#' Runs the load function on the DB within a transaction. Then run a set of
#' tests on the DB and rollback the changes if any should fail.
#'
#' @param load The load function for making the DB changes.
#' @param con Connection to the database.
#' @param extracted_data Extracted data if needed for testing, otherwise can be
#' NULL.
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
run_load <- function(con, load, extracted_data, transformed_data, test_queries,
                     path, test_file, dry_run, log_table, comment) {
  if (is.null(transformed_data)) {
    stop("Cannot run tests as no data has been transformed.")
  }
  log_data <- build_log_data(path, comment)
  verify_log_table(con, log_table, log_data)
  verify_first_run(con, log_table, log_data)
  DBI::dbBegin(con)
  withCallingHandlers(
    do_load(con, load, extracted_data, transformed_data, path, test_file,
            test_queries, log_table, log_data, dry_run),
    error = function(e) {
      DBI::dbRollback(con)
      stop(e)
    }
  )
  invisible(TRUE)
}

do_load <- function(con, load, extracted_data, transformed_data, path,
                    test_file, test_queries, log_table, log_data, dry_run) {
  before <- test_queries(con)
  load(transformed_data, con)
  after <- test_queries(con)
  test_path <- file.path(path, test_file)
  message(sprintf("Running load tests %s", test_path))
  test_results <- run_load_tests(test_path, before, after, extracted_data,
                                 transformed_data, con)
  if (all_passed(test_results)) {
    if (dry_run) {
      DBI::dbRollback(con)
      message("All tests passed, rolling back dry run import.")
    } else {
      message("All tests passed, commiting changes to database.")
      write_log(con, log_table, log_data)
      DBI::dbCommit(con)
    }
  } else {
    stop("Failed to load data - not all tests passed.")
  }
}
