context("run-automatic-load")

test_that("automaitc load can be run", {
  path <- prepare_test_import("example_automatic_load", add_data = TRUE,
                              add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_automatic_load"), db_name = "test")
  import$extract()
  import$transform()
  import$load()

  con <- import$get_connection()
  data <- DBI::dbGetQuery(con,
    "SELECT p.id, p.name, j.job
     FROM people p
     LEFT JOIN jobs j on p.id = j.person")
  expected_data <- data_frame(c(1,2,3),
                              c("Daisy", "Alice", "Bob"),
                              c(NA, "developer", "researcher"))
  colnames(expected_data) <- c("id", "name", "job")
  expect_equal(data, expected_data)
})
