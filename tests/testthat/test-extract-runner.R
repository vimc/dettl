context("load_runner")

testthat::test_that("messages are printed to console when tests are run", {
  extract_func <- function(data, con) {}
  path <- prepare_test_import()
  con <- db_connect("test", path)
  test_dir <- "example_tests"
  test_file <- "connection_extract_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  extracted_data <- run_extract(con, extract_func, test_dir)

  expect_message(test_extract(con, test_dir, test_file, extracted_data),
                 "Running extract tests connection_extract_test.R")

  expect_message(test_extract(con, test_dir, test_file, extracted_data),
                 "All extract tests passed.")
})


testthat::test_that("useful error shown to user if db connection not valid", {
  extract_func <- function(data, con) {}
  path <- prepare_test_import()
  con <- db_connect("test", path)
  DBI::dbDisconnect(con)
  test_dir <- "example_tests"
  test_file <- "connection_extract_test.R"

  expect_error(run_extract(con, extract_func, test_dir),
               "DB connection is not valid cannot extract data")
})

testthat::test_that("useful error shown to user when extract tests fail", {
  extract_func <- function(data, con) {}
  path <- prepare_test_import()
  con <- db_connect("test", path)
  test_dir <- "example_tests"
  test_file <- "failing_extract_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  extracted_data <- run_extract(con, extract_func, test_dir)

  expect_error(test_extract(con, test_dir, test_file, extracted_data),
               "Not all extract tests passed. Fix tests before proceeding")
})
