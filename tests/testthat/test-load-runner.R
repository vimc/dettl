context("load_runner")

testthat::test_that("messages are printed to console when tests are run", {
  load_func <- function(data, con) {}
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name), add = TRUE)
  con <- db_connect("test", ".")
  transformed_data <- list()
  test_queries <- function(con) {}
  path <- "example_tests"
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  expect_message(run_load(con, load_func, transformed_data, test_queries,
                          path = path, test_file = test_file, dry_run = FALSE),
                 "Running load tests example_tests/connection_load_test.R")

  expect_message(run_load(con, load_func, transformed_data, test_queries,
                          path = path, test_file = test_file, dry_run = FALSE),
                 "All tests passed, commiting changes to database.")
})
