context("run-default-load")

test_that("default load can be run", {
  path <- prepare_test_import("example_default_load", add_data = TRUE,
                              add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_default_load"), db_name = "test")
  run_import(import, c("extract", "transform", "load"))

  con <- import$get_connection()
  data <- DBI::dbGetQuery(con,
    "SELECT p.id, p.name, j.job
     FROM people p
     LEFT JOIN jobs j on p.id = j.person")
  expected_data <- data.frame(c(1,2,3),
                              c("Daisy", "Alice", "Bob"),
                              c(NA, "developer", "researcher"),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("id", "name", "job")
  expect_equal(data, expected_data)
})
