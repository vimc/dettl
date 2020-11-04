context("sql-import")

test_that("can run sql only import sqlite", {
  path <- prepare_test_import("example_sql_only")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  dettl_run(file.path(path, "example_sql_only/"), comment = "sql only",
            stage = c("extract", "transform", "load"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  people <- DBI::dbGetQuery(con, "select * from people")
  expect_equal(people$name, "Alice")
  expect_equal(people$age, 25)
  expect_equal(people$height, 175)

  log <- DBI::dbGetQuery(con, "select * from dettl_import_log")
  expect_equal(log$name, "example_sql_only")
})

test_that("can run sql only import postgres", {
  path <- prepare_test_import("example_sql_only", create_db = FALSE)
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  dettl_run(file.path(path, "example_sql_only/"), db_name = "psql_test",
            comment = "sql only", stage = c("extract", "transform", "load"))
  people <- DBI::dbGetQuery(con, "select * from people")
  expect_equal(people$name, "Alice")
  expect_equal(people$age, 25)
  expect_equal(people$height, 175)

  log <- DBI::dbGetQuery(con, "select * from dettl_import_log")
  expect_equal(log$name, "example_sql_only")
})

test_that("can run import with object", {
  path <- prepare_test_import("example_sql_only")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_sql_only"))
  expect_message(import$extract(),
                 "No extract function defined for this import, skipping step")
  expect_message(import$transform(),
                 "No transform function defined for this import, skipping step")
  expect_message(import$load(),
                 "Running load in a transaction:")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  people <- DBI::dbGetQuery(con, "select * from people")
  expect_equal(people$name, "Alice")
  expect_equal(people$age, 25)
  expect_equal(people$height, 175)

  log <- DBI::dbGetQuery(con, "select * from dettl_import_log")
  expect_equal(log$name, "example_sql_only")
})

test_that("can use pre and post load with sql import", {
  path <- prepare_test_import("example_sql_pre_post_load")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  dettl_run(file.path(path, "example_sql_pre_post_load"),
            stage = c("extract", "transform", "load"))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  ## test pre load has been run
  people <- DBI::dbGetQuery(con, "select * from people")
  expect_equal(people$name, c("Alice", "Ed"))
  expect_equal(people$age, c(25, 5))
  expect_equal(people$height, c(175, 75))

  ## post load has been run
  index <- DBI::dbGetQuery(con, "pragma index_list('people')")
  expect_true(nrow(index) == 1)
  expect_equal(index$name, "people_name")

  log <- DBI::dbGetQuery(con, "select * from dettl_import_log")
  expect_equal(log$name, "example_sql_pre_post_load")
})
