context("no-transaction")

test_that("import can be run outside of a transaction", {
  path <- prepare_test_import("example_no_transaction")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_no_transaction"), db_name = "test")
  ## Create tables we want to exist for this import as this is as an append
  ## mode import
  con <- import$get_connection()
  invisible(DBI::dbExecute(con,
     "CREATE TABLE people_transform (
      id     INTEGER PRIMARY KEY NOT NULL,
      name   TEXT NOT NULL,
      age    INTEGER NOT NULL,
      height INTEGER
    )"
  ))

  import$extract()
  import$transform()
  ## Load stage will try to upload people_load table and fail - with
  ## transactions off this should still upload the rest of the data
  expect_error(import$load(),
               "Not importing table 'people_load'")

  ## Preload was run
  tables <- DBI::dbGetQuery(con,
                            "SELECT
                                 name
                             FROM
                                 sqlite_master
                             WHERE
                                 type ='table' AND
                                 name NOT LIKE 'sqlite_%'
  ")$name
  expect_true("people" %in% tables)
  expect_true("people_transform" %in% tables)
  expect_false("people_load" %in% tables)

  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[[1]], 2)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT count(*) from people_transform")[[1]], 3)
})

test_that("run in transaction if dry_run is TRUE and transaction FALSE", {
  path <- prepare_test_import("example_no_transaction")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_no_transaction"), db_name = "test")
  ## Create tables we want to exist for this import as this is as an append
  ## mode import
  con <- import$get_connection()
  invisible(DBI::dbExecute(con,
                           "CREATE TABLE people_transform (
      id     INTEGER PRIMARY KEY NOT NULL,
      name   TEXT NOT NULL,
      age    INTEGER NOT NULL,
      height INTEGER
    )"
  ))

  import$extract()
  import$transform()
  ## Load stage will try to upload people_load table and fail - with
  ## transactions off this should still upload the rest of the data
  expect_error(import$load(dry_run = TRUE),
               "Not importing table 'people_load'")

  ## Import was rolled back
  tables <- DBI::dbGetQuery(con,
                            "SELECT
                                 name
                             FROM
                                 sqlite_master
                             WHERE
                                 type ='table' AND
                                 name NOT LIKE 'sqlite_%'
  ")$name
  expect_true("people" %in% tables)
  expect_true("people_transform" %in% tables)
  expect_false("people_load" %in% tables)

  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[[1]], 0)
  expect_equal(
    DBI::dbGetQuery(con, "SELECT count(*) from people_transform")[[1]], 0)
})
