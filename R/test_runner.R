run_load_tests <- function(path, test_file, before, after) {
  env <- new.env(parent = .GlobalEnv)
  env$before <- before
  env$after <- after
  test_results <- testthat::test_file(file.path(path, test_file), env = env)
}

run_extract_tests <- function(path, test_file, extracted_data) {
  env <- new.env(parent = .GlobalEnv)
  env$extracted_data <- extracted_data
  test_results <- testthat::test_file(file.path(path, test_file), env = env)
}

run_transform_tests <- function(path, test_file, transformed_data) {
  env <- new.env(parent = .GlobalEnv)
  env$transformed_data <- transformed_data
  test_results <- testthat::test_file(file.path(path, test_file), env = env)
}

## return if all tests are successful w/o error
## Taken from testthat source see
## https://github.com/r-lib/testthat/blob/e2703ca962af0419f122eb348b7a216e8861e887/R/reporter-list.R#L76
all_passed <- function(testhat_result) {
  if (length(testhat_result) == 0) {
    return(TRUE)
  }

  df <- as.data.frame(testhat_result)
  sum(df$failed) == 0 && all(!df$error)
}
