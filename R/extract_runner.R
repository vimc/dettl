run_extract <- function(con, extract, path, extract_test){
  if (!is.null(con) && DBI::dbIsValid(con)) {
    extracted_data <- extract(path, con)
  } else {
    stop("DB connection is not valid cannot extract data")
  }
  if (!is.null(extract_test)) {
    test_results <- run_extract_tests(path, extract_test, extracted_data)
    if (!all_passed(test_results)) {
      stop("Not all extract tests passed. Fix tests before proceeding.")
    }
  }
  extracted_data
}
