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
                          path = path, test_file = test_file, dry_run = FALSE,
                          log_table = "log_table", comment = NULL),
                 "Running load tests example_tests/connection_load_test.R")

  expect_message(run_load(con, load_func, transformed_data, test_queries,
                          path = path, test_file = test_file, dry_run = FALSE,
                          log_table = "log_table", comment = NULL),
                 "All tests passed, commiting changes to database.")
})

testthat::test_that("log table is appended to", {
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

  run_load(con, load_func, transformed_data, test_queries, path = path,
           test_file = test_file, dry_run = FALSE, log_table = "log_table",
           comment = "Test comment")
  log_data <- DBI::dbGetQuery(con, "SELECT * FROM log_table")
  expect_true(nrow(log_data) == 1)
  expect_equal(log_data$name, "example_tests")
  ## We want to check logged time is within some reasonable range.
  ## Arbitrarily choose 1 min ago.
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(log_data$date))
  expect_true(as.numeric(log_data$date) < as.numeric(Sys.time()))
  expect_equal(log_data$comment, "Test comment")
})

testthat::test_that("postgres log table is appended to", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))

  load_func <- function(data, con) {}
  transformed_data <- list()
  test_queries <- function(con) {}
  path <- "example_tests"
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  run_load(con, load_func, transformed_data, test_queries, path = path,
           test_file = test_file, dry_run = FALSE, log_table = "log_table",
           comment = "Test comment")
  log_data <- DBI::dbGetQuery(con, "SELECT * FROM log_table")
  expect_true(nrow(log_data) == 1)
  expect_equal(log_data$name, "example_tests")
  ## We want to check logged time is within some reasonable range.
  ## Arbitrarily choose 1 min ago.
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(log_data$date))
  expect_true(as.numeric(log_data$date) < as.numeric(Sys.time()))
  expect_equal(log_data$comment, "Test comment")
})
