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
run_load <- function(connection, load, extracted_data, transformed_data,
                     test_queries, pre_load, post_load, path, test_file,
                     transaction, dry_run, log_table, comment) {
  if (is.null(transformed_data)) {
    stop("Cannot run tests as no data has been transformed.")
  }
  log_data <- build_log_data(path, comment)
  verify_log_table(connection$con, log_table, log_data)
  verify_first_run(connection$con, log_table, log_data)
  use_transaction <- transaction || dry_run
  if (use_transaction) {
    connection$begin()
  }
  withCallingHandlers(
    do_load(connection, load, extracted_data, transformed_data, path, test_file,
            test_queries, pre_load, post_load, log_table, log_data, transaction,
            dry_run),
    error = function(e) {
      if (use_transaction) {
        connection$rollback()
      } else {
        message("ATTENTION: even though your load has failed, because you did not use a transaction, the database may have been modified")
      }
      stop(e)
    }
  )
  invisible(TRUE)
}

do_load <- function(connection, load, extracted_data, transformed_data, path,
                    test_file, test_queries, pre_load, post_load, log_table,
                    log_data, transaction, dry_run) {
  message(
    sprintf("Running load %s:",
            transaction %?% "in a transaction" %:% "not in a transaction"))
  message("\t- Running test queries before making any changes")
  withr::with_dir(path, {
    before <- test_queries(connection$con)
    if (!is.null(pre_load)) {
      message("\t- Running pre-load")
      pre_load(transformed_data, connection$con)
    }
    message("\t- Running load step")
    load(transformed_data, connection$con)
    if (!is.null(post_load)) {
      message("\t- Running post-load")
      post_load(transformed_data, connection$con)
    }
    message("\t- Running test queries after making changes")
    after <- test_queries(connection$con)
    message(sprintf("\t- Running load tests %s", test_file))
    test_results <- run_load_tests(test_file, before, after, extracted_data,
                                   transformed_data, connection$con)
  })
  if (all_passed(test_results)) {
    if (dry_run) {
      connection$rollback()
      message("All tests passed, rolling back dry run import.")
    } else {
      message("All tests passed, commiting changes to database.")
      write_log(connection$con, log_table, log_data)
      if (transaction) {
        connection$commit()
      }
    }
  } else {
    stop("Failed to load data - not all tests passed.")
  }
}
