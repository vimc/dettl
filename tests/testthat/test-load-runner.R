context("load_runner")

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
    run_load(con, load_func, extracted_data = NULL, transformed_data,
             test_queries, pre_load = NULL, post_load = NULL, path = test_dir,
             test_file = test_file, log_table = "table log",
             comment = "Test comment"),
    "Cannot import data: Table 'table log' is missing from db schema. Please run dettl::dettl_create_log_table first."
  )

  invisible(DBI::dbExecute(con,
    "CREATE TABLE log_table (
      name   TEXT
    )"
  ))
  mock_build_log_data <- mockery::mock(data_frame(name = "test"))
  with_mock("dettl:::build_log_data" = mock_build_log_data, {
    expect_error(
      run_load(con, load_func, extracted_data = NULL, transformed_data,
               test_queries, pre_load = NULL, post_load = NULL, path = test_dir,
               test_file = test_file, log_table = "log_table",
               comment = "Test comment"),
      "Cannot import data: Column 'start_time' is missing from db schema."
    )
  })
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
    run_load(con, dettl_auto_load, extracted_data = NULL, transformed_data,
             test_queries, pre_load = NULL, post_load = NULL,
             path = test_dir, test_file = test_file,
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
    run_load(con, dettl_auto_load, extracted_data = NULL, transformed_data,
             test_queries, pre_load = NULL, post_load = NULL,
             path = test_dir, test_file = test_file,
             log_table = "dettl_import_log", comment = "Test comment")
  )

  ## Test that transaction is not currently active - check by trying to start
  ## a new one and ensuring that no error is thrown.
  expect_true(DBI::dbBegin(con))
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
})

test_that("pre and post load functions are called if not NULL", {
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

  mock_pre_load <- mockery::mock(TRUE)
  mock_post_load <- mockery::mock(TRUE)
  run_load(con, load_func, extracted_data = NULL, transformed_data,
           test_queries, pre_load = mock_pre_load, post_load = mock_post_load,
           path = test_dir, test_file = test_file,
           log_table = "dettl_import_log", comment = NULL)

  mockery::expect_called(mock_pre_load, 1)
  mockery::expect_called(mock_post_load, 1)
})
