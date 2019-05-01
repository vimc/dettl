context("test-dettl-runner")

test_that("dettl works as expected", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## when creating import object
  import <- dettl(file.path(path, "example/"), db_name = "test")

  ## object has been created
  expect_false(is.null(import))
  expect_is(import, "DataImport")

  ## and connection and DB have been setup
  con <- import$get_connection()
  expect_true(!is.null(con) && DBI::dbIsValid(con))
  expect_true("people" %in% DBI::dbListTables(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## and no data has been extracted or transformed
  extracted_data <- import$get_extracted_data()
  expect_null(extracted_data, "Expected data non null")
  transformed_data <- import$get_transformed_data()
  expect_null(transformed_data, "Transformed data is non-null")

  ## when data is extracted
  import <- run_import(import, "extract")
  extracted_data <- import$get_extracted_data()
  expected_data <- data.frame(c("Alice", "Bob", "Clive"),
                              c(25, 43, 76),
                              c(175, 187, 163),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("name", "age", "height")

  ## data has been read from files
  expect_equal(length(extracted_data), 1)
  expect_equal(extracted_data$people, expected_data)

  ## transformed data is still null
  transformed_data <- import$get_transformed_data()
  expect_null(transformed_data, "Transformed data is non-null")

  ## when running transform
  import <- run_import(import, "transform")
  transformed_data <- import$get_transformed_data()

  ## transform data is available
  expect_equal(length(transformed_data), 1)
  expect_equal(transformed_data$people, expected_data[c(1,2), ])

  ## and DB is still empty
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## when load is run
  import <- run_import(import, "load")

  ## then database contains correct data
  expect_equal(DBI::dbGetQuery(con, "SELECT name, age, height from people"), expected_data[c(1,2), ])
})

test_that("import can be created using a default db", {
  path <- prepare_test_import()

  import <- dettl(file.path(path, "example/"))
  con <- import$get_connection()
  expect_equal(con@dbname, file.path(path, "test.sqlite"))
})

test_that("run import runs a full import process", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), db_name = "test")
  import <- run_import(import)
  con <- import$get_connection()
  expected_data <- data.frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("name", "age", "height")
  expect_equal(DBI::dbGetQuery(con, "SELECT name, age, height from people"), expected_data)
})

test_that("run step rolls back when tests fail", {
  path <- prepare_test_import("example_failing_test")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_failing_test/"), db_name = "test")
  expect_error(run_import(import),
               "Failed to load data - not all tests passed.")

})

test_that("transform cannot be run until extract stage has been run", {
  path <- prepare_test_import()

  import <- dettl(file.path(path, "example/"), db_name = "test")

  expect_error(import$transform(),
               "Cannot run transform as no data has been extracted.")
})

test_that("load cannot be run until transform stage has been run", {
  path <- prepare_test_import()

  import <- dettl(file.path(path, "example/"), db_name = "test")

  expect_error(import$load(),
               "Cannot run tests as no data has been transformed.")
})

test_that("import cannot be run on object of wrong type", {

  expect_error(
    run_import(NULL),
    "Can only run import for non null data import with class 'DataImport'."
  )

  path <- prepare_test_import()

  import <- dettl(file.path(path, "example/"), db_name = "test")
  class(import) <- "data_import"

  expect_error(
    run_import(import),
    "Can only run import for non null data import with class 'DataImport'."
  )
})

test_that("trying to create import for db missing from config fails", {

  expect_error(dettl("example/", db_name = "missing"),
              "Cannot find config for database missing.")
})

test_that("a dry run of the import can be executed", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), db_name = "test")
  con <- import$get_connection()

  ## when running extract + transform as a dry run
  import <- run_import(import, c("extract", "transform"), dry_run = TRUE)
  transformed_data <- import$get_transformed_data()

  ## transform is unchanged
  expected_data <- data.frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("name", "age", "height")
  expect_equal(length(transformed_data), 1)
  expect_equal(transformed_data$people, expected_data[c(1,2), ])
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## when load is run as a dry run
  import <- run_import(import, "load", dry_run = TRUE)

  ## then database has not been updated
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)
})

test_that("run import prints import directory to the log", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), db_name = "test")
  expect_message(run_import(import, "extract"),
    sprintf("Running import %s", file.path(path, "example")))
})

test_that("run import checks git state before import is run", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## Add a new file to test that git is checked
  writeLines("test", file.path(path, "test"))
  import <- dettl(file.path(path, "example/"), db_name = "test")
  expect_error(run_import(import, "extract"),
    sprintf("Can't run import as repository has unstaged changes. Update git or run in dry-run mode."))

  ## Import can be run in dry-run mode still
  run_import(import, "extract", dry_run = TRUE)
  expect_true(!is.null(import$get_extracted_data()))

  ## Import can skip git checks using force
  run_import(import, "transform", force = TRUE)
  expect_true(!is.null(import$get_transformed_data()))
})
