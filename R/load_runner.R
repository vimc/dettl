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
#' @param comment An optional comment to add to the import log table for this
#' run.
#'
#' @keywords internal
#'
run_load <- function(con, load, extracted_data, transformed_data, test_queries,
                     pre_load, post_load, path, test_file, log_table, comment) {
  start_time <- get_time()
  log_data <- build_log_data(path, comment)
  verify_log_table(con, log_table, log_data)
  verify_first_run(con, log_table, log_data)
  withr::with_dir(path,
    do_load(con, load, extracted_data, transformed_data, test_file,
            test_queries, pre_load, post_load))
  log_data$start_time <- start_time
  log_data$end_time <- get_time()
  ## Save the duration in seconds rounded to at most precise the time in ms
  log_data$duration <- round(
    as.numeric(log_data$end_time) - as.numeric(log_data$start_time), digits = 3)
  log_data
}

do_load <- function(con, load, extracted_data, transformed_data, test_file,
                    test_queries, pre_load, post_load) {
  message("\t- Running test queries before making any changes")
  before <- test_queries(con)
  if (!is.null(pre_load)) {
    message("\t- Running pre-load")
    pre_load(transformed_data, con)
  }
  message("\t- Running load step")
  load(transformed_data, con)
  if (!is.null(post_load)) {
    message("\t- Running post-load")
    post_load(transformed_data, con)
  }
  message("\t- Running test queries after making changes")
  after <- test_queries(con)
  message(sprintf("\t- Running load tests %s", test_file))
  test_results <- run_load_tests(test_file, before, after, extracted_data,
                                 transformed_data, con)
  if (!all_passed(test_results)) {
    stop("Failed to load data - not all tests passed.")
  }
}
