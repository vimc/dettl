context("load_runner")

testthat::test_that("messages are printed to console when tests are run", {
  path <- prepare_test_import("example_tests")
  con <- db_connect("test", path)
  load_func <- function(data, con) {}
  transformed_data <- list()
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## Ideally here we would run run_load and check for messages using
  ## expect_message. This doesn't support checking for 2 messages from one call
  ## and calling run_load twice violates the unique key constraint so work
  ## around this by storing the messages in a variable and checking these
  ## individually.
  run_load_call <- function() {
    run_load(con, load_func, extracted_data = NULL, transformed_data, test_queries,
             path = test_dir, test_file = test_file, dry_run = FALSE,
             log_table = "dettl_import_log", comment = NULL)
  }
  res <- evaluate_promise(run_load_call())
  expect_true(any(grepl(sprintf(
    "Running load tests %s/example_tests/connection_load_test.R", path),
    fixed = TRUE, res$messages)))
  expect_true(any(grepl("All tests passed, commiting changes to database.",
    res$messages, fixed = TRUE)))
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

  run_load(con, load_func, extracted_data = NULL, transformed_data, test_queries, path = test_dir,
           test_file = test_file, dry_run = FALSE,
           log_table = "dettl_import_log", comment = "Test comment")
  log_data <- DBI::dbGetQuery(con, "SELECT * FROM dettl_import_log")
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

  run_load(con, load_func, extracted_data = NULL, transformed_data, test_queries, path = test_dir,
           test_file = test_file, dry_run = FALSE,
           log_table = "dettl_import_log", comment = "Test comment")
  log_data <- DBI::dbGetQuery(con, "SELECT * FROM dettl_import_log")
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
    run_load(con, load_func, extracted_data = NULL, transformed_data, test_queries, path = test_dir,
             test_file = test_file, dry_run = FALSE, log_table = "table log",
             comment = "Test comment"),
    "Cannot import data: Table 'table log' is missing from db schema. Please run dettl::dettl_create_log_table first."
  )

  mock_build_log_data <- mockery::mock(data_frame(name = "test"))
  with_mock("dettl:::build_log_data" = mock_build_log_data, {
    expect_error(
      run_load(con, load_func, extracted_data = NULL, transformed_data, test_queries, path = test_dir,
               test_file = test_file, dry_run = FALSE,
               log_table = "dettl_import_log", comment = "Test comment"),
      "Cannot import data: Column 'date' in table 'dettl_import_log' in DB but is missing from local table."
    )
  })
})

test_that("import can only be run once", {
  path <- prepare_test_import("example_tests")
  con <- db_connect("test", path)
  load_func <- function(data, con) {}
  transformed_data <- list()
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  run_load(con, load_func, extracted_data = NULL, transformed_data, test_queries,
           path = test_dir, test_file = test_file,
           dry_run = FALSE, log_table = "dettl_import_log",
           comment = NULL)

  expect_error(run_load(con, load_func, extracted_data = NULL, transformed_data, test_queries,
                        path = test_dir, test_file = test_file,
                        dry_run = FALSE, log_table = "dettl_import_log",
                        comment = NULL),
"Import has previously been run. Previous run log:
  name:           example_tests
  date:           [0-9:\\s-]+
  comment:        NA
  git user.name:  dettl
  git user.email: email@example.com
  git branch:     master
  git hash:       \\w+", perl = TRUE)
})

test_that("transaction is cleaned up if import fails", {
  path <- prepare_test_import("example_tests")
  con <- db_connect("test", path)
  transformed_data <- list(people = "Dave")
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## Error thrown from bad form of transformed_data
  expect_error(
    run_load(con, dettl_auto_load, extracted_data = NULL, transformed_data, test_queries,
             path = test_dir, test_file = test_file, dry_run = FALSE,
             log_table = "dettl_import_log", comment = "Test comment")
  )

  ## Test that transaction is not currently active - check by trying to start
  ## a new one and ensuring that no error is thrown.
  expect_true(DBI::dbBegin(con))
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
})

test_that("postgres transaction is cleaned up if import throws error", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))
  path <- prepare_test_import("example_tests")

  transformed_data <- list(people = "Dave")
  test_queries <- function(con) {}
  test_dir <- file.path(path, "example_tests")
  test_file <- "connection_load_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## Error thrown from bad form of transformed_data
  expect_error(
    run_load(con, dettl_auto_load, extracted_data = NULL, transformed_data, test_queries,
             path = test_dir, test_file = test_file, dry_run = FALSE,
             log_table = "dettl_import_log", comment = "Test comment")
  )

  ## Test that transaction is not currently active - check by trying to start
  ## a new one and ensuring that no error is thrown.
  expect_true(DBI::dbBegin(con))
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
})
