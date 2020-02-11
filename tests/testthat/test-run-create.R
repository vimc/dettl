context("run-create")

test_that("can run a create mode update", {
  path <- prepare_test_import("example_create_table")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_create_table"), db_name = "test")
  import$extract()
  import$transform()
  import$load()

  con <- import$get_connection()
  data <- DBI::dbGetQuery(con, "SELECT * FROM hobbies")
  expected_data <- data_frame(c("Alice", "Bob", "Clive"),
                              c("acupuncture", "baking", "crime"))
  colnames(expected_data) <- c("name", "hobby")
  expect_equal(data, expected_data)


  abode <- DBI::dbGetQuery(con, "SELECT * FROM abode")
  expected_abode <- data_frame(c("Alice", "Bob"),
                               c("house", "flat"))
  colnames(expected_abode) <- c("person", "type")
  expect_equal(abode, expected_abode)
})
