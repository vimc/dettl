run_load_tests <- function(path, before, after, con,
                           reporter = testthat::default_reporter()) {
  env <- new.env(parent = .GlobalEnv)
  env$before <- before
  env$after <- after
  env$con <- con
  test_results <- testthat::test_file(path, env = env, reporter = reporter)
}

run_extract_tests <- function(path, extracted_data, con,
                              reporter = testthat::default_reporter()) {
  env <- new.env(parent = .GlobalEnv)
  env$extracted_data <- extracted_data
  env$con <- con
  test_results <- testthat::test_file(path, env = env, reporter = reporter)
}

run_transform_tests <- function(path, transformed_data, extracted_data, con,
                                reporter = testthat::default_reporter()) {
  env <- new.env(parent = .GlobalEnv)
  env$transformed_data <- transformed_data
  env$extracted_data <- extracted_data
  env$con <- con
  test_results <- testthat::test_file(path, env = env, reporter = reporter)
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
