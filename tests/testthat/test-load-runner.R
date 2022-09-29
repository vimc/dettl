context("load_runner")

testthat::test_that("import fails if log table misconfigured", {
  config_path <- setup_config(log_table = "table_log")
  path <- prepare_test_import(dettl_config =
                                file.path(config_path, "dettl_config.yml"))

  expect_error(dettl(file.path(path, "example/"), "example"), paste0(
    "Cannot import data: Table 'table_log' is missing from db schema. ",
    "Please run dettl::dettl_create_log_table first."))

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  invisible(DBI::dbExecute(con,
    "CREATE TABLE table_log (
      name   TEXT,
      language TEXT,
      mode TEXT,
      comment TEXT,
      git_user TEXT,
      git_email TEXT,
      git_branch TEXT,
      git_hash TEXT
    )"
  ))
  expect_error(
    dettl(file.path(path, "example/"), "example"),
    "Cannot import data: Column 'start_time' is missing from db schema.")
})

test_that("transaction is cleaned up if import fails", {
  path <- prepare_test_import()
  import <- dettl(file.path(path, "example/"))

  ## Add some bad formed transformed data to trigger an error
  import$.__enclos_env__$private$transformed_data <- list(people = "Dave")
  expect_error(import$load())

  ## Test that transaction is not currently active - check by trying to start
  ## a new one and ensuring that no error is thrown.
  con <- import$get_connection()
  expect_true(DBI::dbBegin(con))
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
})

test_that("postgres transaction is cleaned up if import throws error", {
  path <- prepare_test_import()
  prepare_example_postgres_db()
  import <- dettl(file.path(path, "example/"), "psql_test")

  ## Add some bad formed transformed data to trigger an error
  import$.__enclos_env__$private$transformed_data <- list(people = "Dave")
  expect_error(import$load())

  ## Test that transaction is not currently active - check by trying to start
  ## a new one and ensuring that no error is thrown.
  con <- import$get_connection()
  expect_true(DBI::dbBegin(con))
  on.exit(DBI::dbRollback(con), add = TRUE, after = FALSE)
})
