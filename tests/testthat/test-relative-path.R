context("relative-path")

test_that("extract, transform and load steps use paths relative to import", {
  path <- prepare_test_import("example_relative_path")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_relative_path"), db_name = "test")
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
  invisible(DBI::dbExecute(con,
                 "CREATE TABLE people_load (
      id     INTEGER PRIMARY KEY NOT NULL,
      name   TEXT NOT NULL,
      age    INTEGER NOT NULL,
      height INTEGER
    )"
  ))

  import$extract()
  import$transform()
  import$load()

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
  expect_true("people_load" %in% tables)
})
