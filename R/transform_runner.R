run_transform <- function(con, transform, path, extracted_data, transform_test){
  if (is.null(extracted_data)) {
    stop("Cannot run transform as no data has been extracted.")
  }
  transformed_data <- transform(extracted_data)
  ## ...check that data looks sensible...
  if (length(transformed_data) == 0) {
    stop("Data transform failed, returned empty list.")
  }
  if (!is.null(transform_test)) {
    test_results <- run_transform_tests(path, transform_test, transformed_data)
    if (!all_passed(test_results)) {
      stop("Not all transform tests passed. Fix tests before proceeding.")
    }
  }
  transformed_data
}
