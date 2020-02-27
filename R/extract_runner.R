#' Run extract step.
#'
#' Step is responsible for reading data from local files and DB needed for
#' the transform step.
#'
#' @param con The active DB connection.
#' @param extract Extract function loaded from config.
#' @param path Path to the project directory, for locating the transform tests
#'
#' @return The extracted data.
#' @keywords internal
#'
run_extract <- function(con, extract, path) {
  if (!is.null(con) && DBI::dbIsValid(con)) {
    withr::with_dir(path, {
      extracted_data <- extract(con)
    })
  } else {
    stop("DB connection is not valid cannot extract data")
  }
  extracted_data
}


#' Run tests for the extract step.
#'
#' Runs any configured user defined tests on the extracted data. The tests are
#' optional for this stage and this always returns TRUE if there are no tests.
#'
#' @param con The active DB connection.
#' @param path Path to the project directory, for locating the transform tests.
#' @param extract_test Optional path to the extract tests. Relative to path
#' argument.
#' @param extracted_data The extracted data to test.
#'
#' @return TRUE if tests pass, otherwise throws an error.
#' @keywords internal
#'
test_extract <- function(con, path, extract_test, extracted_data) {
  if (!is.null(extract_test)) {
    message(sprintf("Running extract tests %s", extract_test))
    test_path <- file.path(path, extract_test)
    test_results <- run_extract_tests(test_path, extracted_data, con)
    if (!all_passed(test_results)) {
      stop("Not all extract tests passed. Fix tests before proceeding.")
    } else {
      message("All extract tests passed.")
    }
  }
  invisible(TRUE)
}
