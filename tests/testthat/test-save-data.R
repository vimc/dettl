context("save-data")

test_that("extracted data can be saved", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name), add = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl("example/", "test")

  file <- tempfile(fileext = ".xlsx")
  expect_error(save_extracted_data(import, file),
               "Can't save extracted data as stage has not been run.")

  run_import(import, "extract")
  expect_message(save_extracted_data(import, file),
                 sprintf("Saved extracted data to %s", file))

  expect_equal(readxl::excel_sheets(file), "people")
  xl_data <- readxl::read_excel(file, sheet = "people")
  expect_equal(colnames(xl_data), c("name", "age", "height"))
  expect_equal(nrow(xl_data), 3)
})

test_that("transformed data can be saved", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name), add = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl("example/", "test")

  file <- tempfile(fileext = ".xlsx")
  expect_error(save_transformed_data(import, file),
    "Can't save transformed data as stage has not been run.")

  run_import(import, c("extract", "transform"))
  save_transformed_data(import, file)

  expect_equal(readxl::excel_sheets(file), "people")
  xl_data <- readxl::read_excel(file, sheet = "people")
  expect_equal(colnames(xl_data), c("name", "age", "height"))
  expect_equal(nrow(xl_data), 2)
})

test_that("trying to save data with non data import object fails", {
  expect_error(save_transformed_data("test"),
               "Can't save transformed data for non-DataImport object.")
})

test_that("save data can create new file", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name), add = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl("example/", "test")
  run_import(import, "extract")

  dir <- tempdir()
  file <- file.path(dir, "test.xlsx")
  save_extracted_data(import, file)
  expect_true("test.xlsx" %in% list.files(dir))
})

test_that("saving data with multiple sheets is supported", {
  db_name <- "test.sqlite"
  prepare_example_db(db_name, add_data = TRUE, add_job_table = TRUE)
  on.exit(unlink(db_name))

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl("example_default_load/", db_name = "test")
  run_import(import, c("extract", "transform"))

  file <- tempfile(fileext = "xlsx")
  save_extracted_data(import, file)

  expect_equal(readxl::excel_sheets(file), c("people", "jobs"))
  people <- readxl::read_excel(file, sheet = "people")
  expect_equal(colnames(people), c("id", "name", "age", "height"))
  expect_equal(nrow(people), 3)
  jobs <- readxl::read_excel(file, sheet = "jobs")
  expect_equal(colnames(jobs), c("person", "job"))
  expect_equal(nrow(jobs), 3)
})
