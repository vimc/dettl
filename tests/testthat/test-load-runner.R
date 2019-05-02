context("load_runner")

testthat::test_that("messages are printed to console when tests are run", {
  load_func <- function(data, con) {}
  path <- prepare_test_import("example_tests")
  con <- db_connect("test", path)
  transformed_data <- list()
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  expect_message(run_load(con, load_func, transformed_data, test_queries,
                          path = test_dir, test_file = test_file,
                          dry_run = FALSE, log_table = "log_table",
                          comment = NULL), sprintf(
                 "Running load tests %s/example_tests/connection_load_test.R",
                 path))

  expect_message(run_load(con, load_func, transformed_data, test_queries,
                          path = test_dir, test_file = test_file,
                          dry_run = FALSE, log_table = "log_table",
                          comment = NULL),
                 "All tests passed, commiting changes to database.")
})

testthat::test_that("log table is appended to", {
  load_func <- function(data, con) {}
  path <- prepare_test_import("example_tests")
  con <- db_connect("test", path)
  transformed_data <- list()
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  run_load(con, load_func, transformed_data, test_queries, path = test_dir,
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
  expect_equal(log_data$git_user, "dettl")
  expect_equal(log_data$git_email, "email@example.com")
  expect_equal(log_data$git_hash, git_hash(path))
  expect_equal(log_data$git_branch, "master")
})

testthat::test_that("postgres log table is appended to", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))
  path <- prepare_test_import("example_tests")

  load_func <- function(data, con) {}
  transformed_data <- list()
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  run_load(con, load_func, transformed_data, test_queries, path = test_dir,
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
  expect_equal(log_data$git_user, "dettl")
  expect_equal(log_data$git_email, "email@example.com")
  expect_equal(log_data$git_hash, git_hash(path))
  expect_equal(log_data$git_branch, "master")
})

testthat::test_that("import fails if log table misconfigured", {
  load_func <- function(data, con) {}
  path <- prepare_test_import("example_tests")
  con <- db_connect("test", path)
  transformed_data <- list()
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  expect_error(
    run_load(con, load_func, transformed_data, test_queries, path = test_dir,
             test_file = test_file, dry_run = FALSE, log_table = "table log",
             comment = "Test comment"),
    "Cannot import data: Table 'table log' is missing from db schema."
  )

  mock_get_log_data <- mockery::mock(
    data.frame(name = "test", stringsAsFactors = FALSE))
  with_mock("dettl:::get_log_data" = mock_get_log_data, {
    expect_error(
      run_load(con, load_func, transformed_data, test_queries, path = test_dir,
               test_file = test_file, dry_run = FALSE, log_table = "log_table",
               comment = "Test comment"),
      "Cannot import data: Column 'date' in table 'log_table' in DB but is missing from local table."
    )
  })
})
