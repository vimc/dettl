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
  con <- DBI::dbConnect(file.path(path, "test.sqlite"))
  people <- DBI::dbGetQuery(con, "select * from people")
  expect_equal(people, "alice")
})

test_that("can run sql only import postgres", {

})

test_that("can run import with object", {
  ## Call extract, transform, load individually and test works
})

test_that("can use pre and post load with sql import", {
  ## These can be a sql file too?
})
