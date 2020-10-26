context("load_runner")

testthat::test_that("import fails if log table misconfigured", {
  config_path <- setup_config(log_table = "table_log")
  path <- prepare_test_import(dettl_config =
                                file.path(config_path, "dettl_config.yml"))

  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), "example")
  import$extract()
  import$transform()
  expect_error(import$load(), paste0(
    "Cannot import data: Table 'table_log' is missing from db schema. ",
    "Please run dettl::dettl_create_log_table first."))

  con <- import$get_connection()
  invisible(DBI::dbExecute(con,
    "CREATE TABLE table_log (
      name   TEXT,
      comment TEXT,
      git_user TEXT,
      git_email TEXT,
      git_branch TEXT,
      git_hash TEXT
    )"
  ))
  expect_error(
    import$load(),
    "Cannot import data: Column 'start_time' is missing from db schema.")

})

test_that("transaction is cleaned up if import fails", {
  path <- prepare_test_import()
  import <- dettl(file.path(path, "example/"))

  ## Add some bad formed transformed data to trigger an error
  mock_private(import, "transformed_data", list(people = "Dave"))
  expect_error(import$load())

  ## Test that transaction is not currently active - check by trying to start
  ## a new one and ensuring that no error is thrown.
  con <- import$get_connection()
  expect_true(DBI::dbBegin(con))
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
})

test_that("postgres transaction is cleaned up if import throws error", {
  path <- prepare_test_import()
  import <- dettl(file.path(path, "example/"), "psql_test")

  ## Add some bad formed transformed data to trigger an error
  mock_private(import, "transformed_data", list(people = "Dave"))
  expect_error(import$load())

  ## Test that transaction is not currently active - check by trying to start
  ## a new one and ensuring that no error is thrown.
  con <- import$get_connection()
  expect_true(DBI::dbBegin(con))
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
})

test_that("pre and post load functions are called if not NULL", {
  path <- prepare_test_import()
  import <- dettl(file.path(path, "example/"))

  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  mock_pre_load <- mockery::mock(TRUE)
  mock_post_load <- mockery::mock(TRUE)
  mock_private(import, "pre_load", mock_pre_load)
  mock_private(import, "has_pre_load", TRUE)
  mock_private(import, "post_load", mock_post_load)
  mock_private(import, "has_post_load", TRUE)

  import$extract()
  import$transform()
  import$load()

  mockery::expect_called(mock_pre_load, 1)
  mockery::expect_called(mock_post_load, 1)
})
