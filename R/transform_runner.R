#' Run transform step.
#'
#' Step is responsible for taking extracted data, running the configured
#' transform function on the data.
#'
#' @param transform Transform function loaded from config.
#' @param extracted_data Data returned from the extract step.
#' @param extracted_passed TRUE if the extract stage tests passed successfully
#'
#' @return The transformed data.
#' @keywords internal
#'
run_transform <- function(transform, extracted_data, extract_passed) {
  if (is.null(extracted_data)) {
    stop("Cannot run transform as no data has been extracted.")
  }
  if (!extract_passed) {
    stop("Cannot run transform as extract tests failed.")
  }
  transform(extracted_data)
}

#' Run tests for transform step.
#'
#' This checks the transformed data adheres to the db schema and runs any
#' configured user defined tests.
#'
#' @param con The active DB connection.
#' @param path Path to the project directory, for locating the transform tests.
#' @param mode The type of import being run.
#' @param transform_test Optional path to the transform tests. Relative to path
#' argument.
#' @param transformed_data Data returned from the transform stage.
#' @param extracted_data Data returned from the extract step.
#'
#' @return TRUE is tests pass, otherwise throws an error.
#' @keywords internal
#'
test_transform <- function(con, path, mode, transform_test, transformed_data,
                           extracted_data) {
  verify_data(con, transformed_data, mode)
  if (!is.null(transform_test)) {
    message(sprintf("Running transform tests %s", transform_test))
    test_path <- file.path(path, transform_test)
    test_results <-
      run_transform_tests(test_path, transformed_data, extracted_data, con)
    if (!all_passed(test_results)) {
      stop("Not all transform tests passed. Fix tests before proceeding.")
    } else {
      message("All transform tests passed.")
    }
  }
  invisible(TRUE)
}


#' Verify the transformed data adhered to the DB schema.
#'
#' Transformed data returns a list of data frames. Ensure that each item of the
#' list matches a table in the DB and has only columns which exist in the DB.
#' Check that table exists in skipped if mode is 'create'.
#'
#' @param con The active DB connection to check the schema for.
#' @param transformed_data The transformed data.
#' @param mode The type of import run.
#'
#' @keywords internal
#'
verify_data <- function(con, transformed_data, mode) {
  if (length(transformed_data) == 0) {
    stop("Data transform failed, returned empty list.")
  }
  if (!allow_create_table(mode)) {
    for (table_name in names(transformed_data)) {
      verify_table(con, table_name, transformed_data[[table_name]],
                   context_info = "Transformed data")
    }
  }
  invisible(TRUE)
}

