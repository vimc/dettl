context("load_runner")

testthat::test_that("messages are printed to console when tests are run", {
  extract_func <- function(data, con) {}
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name), add = TRUE)
  con <- db_connect("test", ".")
  path <- "example_tests"
  test_file <- "connection_extract_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  expect_message(run_extract(con, extract_func, path, test_file),
                 "Running extract tests connection_extract_test.R")

  expect_message(run_extract(con, extract_func, path, test_file),
                 "All extract tests passed.")
})
