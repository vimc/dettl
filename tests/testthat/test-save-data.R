context("save-data")

test_that("extracted data can be saved", {
  path <- prepare_test_import(add_data = TRUE, add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), "test")

  file <- temp_file(fileext = ".xlsx")
  expect_error(dettl_save(import, file, "extract"),
               "Can't save extract data as stage has not been run.")

  import$extract()
  expect_message(dettl_save(import, file, "extract"),
                 sprintf("Saved extract data to %s", file), fix = TRUE)

  expect_equal(readxl::excel_sheets(file), "people")
  xl_data <- readxl::read_excel(file, sheet = "people")
  expect_equal(colnames(xl_data), c("name", "age", "height"))
  expect_equal(nrow(xl_data), 3)
})

test_that("transformed data can be saved", {
  path <- prepare_test_import(add_data = TRUE, add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), "test")

  file <- temp_file(fileext = ".xlsx")
  expect_error(dettl_save(import, file, "transform"),
    "Can't save transform data as stage has not been run.")

  import$extract()
  import$transform()
  dettl_save(import, file, "transform")

  expect_equal(readxl::excel_sheets(file), "people")
  xl_data <- readxl::read_excel(file, sheet = "people")
  expect_equal(colnames(xl_data), c("name", "age", "height"))
  expect_equal(nrow(xl_data), 2)
})

test_that("trying to save data with non data import object fails", {
  t <- temp_file()
  expect_error(dettl_save("test", t, "transform"),
               "Can't save transform data for non-DataImport object.")
})

test_that("save data can create new file", {
  path <- prepare_test_import(add_data = TRUE, add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), "test")
  import$extract()

  dir <- temp_dir()
  file <- file.path(dir, "test.xlsx")
  dettl_save(import, file, "extract")
  expect_true("test.xlsx" %in% list.files(dir))
})

test_that("saving data with multiple sheets is supported", {
  path <- prepare_test_import("example_automatic_load",
                              add_data = TRUE, add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_automatic_load/"), db_name = "test")
  import$extract()
  import$transform()

  file <- temp_file(fileext = "xlsx")
  dettl_save(import, file, "extract")

  expect_equal(readxl::excel_sheets(file), c("people", "jobs"))
  people <- readxl::read_excel(file, sheet = "people")
  expect_equal(colnames(people), c("id", "name", "age", "height"))
  expect_equal(nrow(people), 3)
  jobs <- readxl::read_excel(file, sheet = "jobs")
  expect_equal(colnames(jobs), c("person", "job"))
  expect_equal(nrow(jobs), 3)
})

test_that("saving data can save extract and transform at same time", {
  path <- prepare_test_import("example_automatic_load",
                              add_data = TRUE, add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_automatic_load/"), db_name = "test")
  import$extract()
  import$transform()

  file <- temp_file(fileext = "xlsx")
  dettl_save(import, file, c("extract", "transform"))

  expect_equal(readxl::excel_sheets(file),
               c("extracted_people", "extracted_jobs",
                 "transformed_people", "transformed_jobs"))

   extr_people <- readxl::read_excel(file, sheet = "extracted_people")
  expect_equal(colnames(extr_people), c("id", "name", "age", "height"))
  expect_equal(nrow(extr_people), 3)
  extr_jobs <- readxl::read_excel(file, sheet = "extracted_jobs")
  expect_equal(colnames(extr_jobs), c("person", "job"))
  expect_equal(nrow(extr_jobs), 3)

  trans_people <- readxl::read_excel(file, sheet = "transformed_people")
  expect_equal(colnames(trans_people), c("id", "name", "age", "height"))
  expect_equal(nrow(trans_people), 2)
  trans_jobs <- readxl::read_excel(file, sheet = "transformed_jobs")
  expect_equal(colnames(trans_jobs), c("person", "job"))
  expect_equal(nrow(trans_jobs), 2)
})
