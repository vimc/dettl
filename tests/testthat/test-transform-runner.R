context("transform-runner")

testthat::test_that("empty verification data fails", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  expect_error(verify_data(con, NULL, "append"),
               "Data transform failed, returned empty list.")

  expect_error(verify_data(con, list(), "append"),
               "Data transform failed, returned empty list.")

  expect_error(verify_data(con, list(), "create"),
               "Data transform failed, returned empty list.")
})

testthat::test_that("verification fails if any tables are missing from db", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  transformed_data <- list("missing_table" = data.frame(c(1, 2), c(3, 4)))
  expect_error(
    verify_data(con, transformed_data, "append"),
    "Transformed data: Table 'missing_table' is missing from db schema."
  )
  expect_true(verify_data(con, transformed_data, "create"))
})

testthat::test_that("verification fails if any rows are missing from db", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  transformed_data <- list("people" = data_frame(
    c("Alice", "Bob", "Clive"),
    c(25, 43, 76),
    c(175, 187, 163)
  ))
  colnames(transformed_data$people) <- c("name", "age", "missing_column")
  expect_error(
    verify_data(con, transformed_data, "append"),
    "Transformed data: Column 'missing_column' in table 'people' but is missing from db schema."
  )
})


testthat::test_that("verification passes if data adheres to schema", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  transformed_data <- list("people" = data_frame(
    c("Alice", "Bob", "Clive"),
    c(25, 43, 76),
    c(175, 187, 163)
  ))
  colnames(transformed_data$people) <- c("name", "age", "height")
  expect_silent(verify_data(con, transformed_data, "append"))
})

testthat::test_that("messages are printed to console when tests are run", {
  transform_func <- function(data, con) {
    list(people = data_frame(
      name = "Test",
      age = 2,
      height = 3
    ))
  }
  path <- prepare_test_import()
  con <- db_connect("test", path)
  test_dir <- "example_tests"
  test_file <- "connection_transform_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)
  data <- list()

  transformed_data <- run_transform(transform_func, data, TRUE, path)
  expect_message(
    test_transform(con, test_dir, "append", test_file, transformed_data, data),
    "Running transform tests connection_transform_test.R")

  expect_message(
    test_transform(con, test_dir, "append", test_file, transformed_data, data),
    "All transform tests passed.")
})

test_that("verification fails if not null constraints violated sqlite", {
  path <- prepare_test_import(add_job_table = TRUE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  transformed_data <- list("people" = data_frame(
    age = c(25, 43, 76)
  ))
  expect_error(
    verify_data(con, transformed_data, "append"),
    paste0("Transformed data: Column 'name' in table 'people' violates not ",
           "null constraint - column missing or contains missing values.")
  )

  transformed_data <- list("people" = data_frame(
    name = c("Alice", NA, "Bob"),
    age = c(25, 43, 76)
  ))
  expect_error(
    verify_data(con, transformed_data, "append"),
    paste0("Transformed data: Column 'name' in table 'people' violates not ",
           "null constraint - column missing or contains missing values.")
  )
})


test_that("verification fails if not null constraints violated postgres", {
  path <- prepare_test_import(create_db = FALSE)
  con <- prepare_example_postgres_db(add_job_table = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  transformed_data <- list("people" = data_frame(
    age = c(25, 43, 76)
  ))
  expect_error(
    verify_data(con, transformed_data, "append"),
    paste0("Transformed data: Column 'name' in table 'people' violates not ",
           "null constraint - column missing or contains missing values.")
  )

  transformed_data <- list("people" = data_frame(
    name = c("Alice", NA, "Bob"),
    age = c(25, 43, 76)
  ))
  expect_error(
    verify_data(con, transformed_data, "append"),
    paste0("Transformed data: Column 'name' in table 'people' violates not ",
           "null constraint - column missing or contains missing values.")
  )
})

testthat::test_that("useful error returned when transform tests fail", {
  transform_func <- function(data, con) {
    list(people = data_frame(
      name = "Test",
      age = 2,
      height = 3
    ))
  }
  path <- prepare_test_import()
  con <- db_connect("test", path)
  test_dir <- "example_tests"
  test_file <- "failing_transform_test.R"
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "Silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)
  data <- list()

  transformed_data <- run_transform(transform_func, data, TRUE, path)
  expect_error(
    test_transform(con, test_dir, "append", test_file, transformed_data, data),
    "Not all transform tests passed. Fix tests before proceeding.")
})

test_that("transform can't be run until extract tests have passed", {
  transform_func <- function(data, con) {
    list(people = data_frame(
      name = "Test",
      age = 2,
      height = 3
    ))
  }
  data <- list()
  expect_error(run_transform(transform_func, data, FALSE),
               "Cannot run transform as extract tests failed.")
})
