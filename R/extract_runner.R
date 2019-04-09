#' Run extract step.
#'
#' Step is responsible for reading data from local files and DB needed for
#' the transform step and running any configured user defined tests on the
#' extracted data.
#'
#' @param con The active DB connection.
#' @param extract Extract function loaded from config.
#' @param path Path to the project directory, for locating the transform tests
#' @param extract_test Optional path to the extract tests. Relative to path
#' argument.
#'
#' @return The extracted data.
#' @keywords internal
#'
run_extract <- function(con, extract, path, extract_test){
  if (!is.null(con) && DBI::dbIsValid(con)) {
    extracted_data <- extract(path, con)
  } else {
    stop("DB connection is not valid cannot extract data")
  }
  if (!is.null(extract_test)) {
    test_path <- file.path(path, extract_test)
    message(sprintf("Running extract tests %s", test_path))
    test_results <- run_extract_tests(test_path, extracted_data, con)
    if (!all_passed(test_results)) {
      stop("Not all extract tests passed. Fix tests before proceeding.")
    } else {
      message("All extract tests passed.")
    }
  }
  extracted_data
}
