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

  file <- tempfile()
  expect_error(save_extracted_data(import, file),
               "Can't save extracted data as stage has not been run.")

  run_import(import, "extract")
  save_extracted_data(import, file)

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

  file <- tempfile()
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

test_that("save data can create new file and save to default location", {
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

  withr::with_dir(dir, {
    save_extracted_data(import)
    expect_true("extracted_data.xlsx" %in% list.files())
  })
})
