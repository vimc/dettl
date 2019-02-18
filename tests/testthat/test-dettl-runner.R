context("test-dettl-runner")

test_that("dettl works as expected", {

  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name), add = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## when creating import object
  import <- new_import("example/")

  ## object has been created
  expect_false(is.null(import))
  expect_equal(class(import), c("dataImport", "R6"))

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
  expect_equal(DBI::dbGetQuery(con, "SELECT * from people"), expected_data[c(1,2), ])
})

test_that("run import runs a full import process", {

  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- new_import("example/")
  import <- run_import(import)
  con <- import$get_connection()
  expected_data <- data.frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("name", "age", "height")
  expect_equal(DBI::dbGetQuery(con, "SELECT * from people"), expected_data)
})

test_that("run step rolls back when tests fail", {

  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- new_import("example_failing_test/")
  expect_error(run_import(import),
               "Failed to load data - not all tests passed.")

})

test_that("transform cannot be run until extract stage has been run", {

  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  import <- new_import("example/")

  expect_error(import$transform(),
               "Cannot run transform as no data has been extracted.")
})

test_that("load cannot be run until transform stage has been run", {

  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  import <- new_import("example/")

  expect_error(import$load(),
               "Cannot run tests as no data has been transformed.")
})

test_that("import cannot be run on object of wrong type", {

  expect_error(
    run_import(NULL),
    "Can only run import for non null data import with class 'dataImport'."
  )

  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  import <- new_import("example/")
  class(import) <- "data_import"

  expect_error(
    run_import(import),
    "Can only run import for non null data import with class 'dataImport'."
  )
})
