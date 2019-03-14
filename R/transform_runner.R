#' Run transform step.
#'
#' Step is responsible for taking extracted data, running the configured
#' transform function on the data, checking the returned data adhers to the
#' db schema and running any configured user defined tests.
#'
#' @param con The active DB connection.
#' @param transform Transform function loaded from config.
#' @param path Path to the project directory, for locating the transform tests
#' @param extracted_data Data returned from the extract step.
#' @param transform_test Optional path to the transform tests. Relative to path
#' argument.
#'
#' @return The successfully transformed data.
#' @keywords internal
#'
run_transform <-
  function(con,
           transform,
           path,
           extracted_data,
           transform_test) {
    if (is.null(extracted_data)) {
      stop("Cannot run transform as no data has been extracted.")
    }
    transformed_data <- transform(extracted_data)
    verify_data(con, transformed_data)
    if (!is.null(transform_test)) {
      test_results <-
        run_transform_tests(path, transform_test, transformed_data, con)
      if (!all_passed(test_results)) {
        stop("Not all transform tests passed. Fix tests before proceeding.")
      }
    }
    transformed_data
  }


#' Verify the transformed data adhered to the DB schema.
#'
#' Transformed data returns a list of data frames. Ensure that each item of the
#' list matches a table in the DB and has only columns which exist in the DB.
#'
#' @param con The active DB connection to check the schema for.
#' @param transformed_data The transformed data.
#'
#' @keywords internal
#'
verify_data <- function(con, transformed_data) {
  if (length(transformed_data) == 0) {
    stop("Data transform failed, returned empty list.")
  }
  for (table_name in names(transformed_data)) {
    if (!DBI::dbExistsTable(con, table_name)) {
      stop(sprintf(
        "Table '%s' returned by transform but is missing from db schema.",
        table_name
      ))
    }
    col_names <- DBI::dbListFields(con, table_name)
    for(col_name in colnames(transformed_data[[table_name]])) {
      if(!(col_name %in% col_names)) {
        stop(sprintf(
          "Column '%s' in table '%s' returned by transform but is missing from db schema.",
          col_name,
          table_name
        ))
      }
    }
  }
  ## TODO: Also check each table doesn't violate any constraints
}

