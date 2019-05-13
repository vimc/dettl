context("transform-runner")

testthat::test_that("empty verification data fails", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  expect_error(verify_data(con, NULL),
               "Data transform failed, returned empty list.")

  expect_error(verify_data(con, list()),
               "Data transform failed, returned empty list.")
})

testthat::test_that("verification fails if any tables are missing from db", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  transformed_data <- list("missing_table" = data.frame(c(1, 2), c(3, 4)))
  expect_error(
    verify_data(con, transformed_data),
    "Transformed data: Table 'missing_table' is missing from db schema."
  )
})

testthat::test_that("verification fails if any rows are missing from db", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  transformed_data <- list("people" = data.frame(
    c("Alice", "Bob", "Clive"),
    c(25, 43, 76),
    c(175, 187, 163),
    stringsAsFactors = FALSE
  ))
  colnames(transformed_data$people) <- c("name", "age", "missing_column")
  expect_error(
    verify_data(con, transformed_data),
    "Transformed data: Column 'missing_column' in table 'people' but is missing from db schema."
  )
})


testthat::test_that("verification passes if data adheres to schema", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  transformed_data <- list("people" = data.frame(
    c("Alice", "Bob", "Clive"),
    c(25, 43, 76),
    c(175, 187, 163),
    stringsAsFactors = FALSE
  ))
  colnames(transformed_data$people) <- c("name", "age", "height")
  expect_silent(verify_data(con, transformed_data))
})

testthat::test_that("messages are printed to console when tests are run", {
  transform_func <- function(data, con) {
    list(people = stats::setNames(
      c("Test", 2, 3),
      c("name", "age", "height")
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

  expect_message(run_transform(con, transform_func, test_dir, data, test_file),
                 "Running transform tests connection_transform_test.R")

  expect_message(run_transform(con, transform_func, test_dir, data, test_file),
                 "All transform tests passed.")
})
