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

  ## and log table is available
  expect_equal(import$get_log_table(), "dettl_import_log")

  ## and no data has been extracted or transformed
  extracted_data <- import$get_extracted_data()
  expect_null(extracted_data, "Expected data non null")
  transformed_data <- import$get_transformed_data()
  expect_null(transformed_data, "Transformed data is non-null")

  ## when data is extracted
  import$extract()
  extracted_data <- import$get_extracted_data()
  expected_data <- data_frame(c("Alice", "Bob", "Clive"),
                              c(25, 43, 76),
                              c(175, 187, 163))
  colnames(expected_data) <- c("name", "age", "height")

  ## data has been read from files
  expect_equal(length(extracted_data), 1)
  expect_equal(extracted_data$people, expected_data)

  ## transformed data is still null
  transformed_data <- import$get_transformed_data()
  expect_null(transformed_data, "Transformed data is non-null")

  ## when running transform
  import$transform()
  transformed_data <- import$get_transformed_data()

  ## transform data is available
  expect_equal(length(transformed_data), 1)
  expect_equal(transformed_data$people, expected_data[c(1,2), ])

  ## and DB is still empty
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## when load is run
  import$load()

  ## then database contains correct data
  expect_equal(DBI::dbGetQuery(con, "SELECT name, age, height from people"),
               expected_data[c(1,2), ])

})

test_that("import can be created using a default db", {
  path <- prepare_test_import()
  import <- dettl(file.path(path, "example/"))
  con <- import$get_connection()
  fs_dbname <- gsub('\\\\', '/', con@dbname)
  expect_equal(normalizePath(fs_dbname),
               normalizePath(file.path(path, "test.sqlite")))
})

test_that("run import runs a full import process", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"), db_name = "test")
  import$extract()
  import$transform()
  import$load()
  con <- import$get_connection()
  expected_data <- data_frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187))
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
  import$extract()
  import$transform()
  expect_error(import$load(),
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

test_that("trying to create import for db missing from config fails", {

  expect_error(dettl(file.path("example/"), db_name = "missing"),
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

  ## when running extract + transform
  import$extract()
  import$transform()
  transformed_data <- import$get_transformed_data()

  ## transformed data is available
  expected_data <- data_frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187))
  colnames(expected_data) <- c("name", "age", "height")
  expect_equal(length(transformed_data), 1)
  expect_equal(transformed_data$people, expected_data[c(1,2), ])
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## when load is run as a dry run
  import$load(dry_run = TRUE)

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
  expect_message(import$extract(),
                 sprintf("Running extract .*example"))
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
  import$extract()
  import$transform()
  expect_error(import$load(),
               sprintf("Can't run load as repository has unstaged changes. Update git or run in dry-run mode."))

  ## Import can be run in dry-run mode still
  import_load <- import$load(dry_run = TRUE)
  expect_true(import_load)

  ## Import can skip git checks using allow_dirty_git
  import_load <- import$load(allow_dirty_git = TRUE)
  expect_true(import_load)
})

test_that("run import asks to confirm run if configured", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## Mock dettl_config return
  config <- dettl_config(file.path(path, "example/"))
  config$db[["test"]]$confirm <- TRUE
  mock_confim_config <- mockery::mock(config, cycle = TRUE)
  mock_no_answer <- mockery::mock(FALSE, cycle = TRUE)
  mock_NA_answer <- mockery::mock(NA, cycle = TRUE)
  mock_yes_answer <- mockery::mock(TRUE, cycle = TRUE)

  ## Set up promise for checking returned messages
  fn <- function(import) {
    import$load()
  }

  with_mock("dettl:::dettl_config" = mock_confim_config,
            "askYesNo" = mock_no_answer, {
              import <- dettl(file.path(path, "example/"), db_name = "test")
              import$extract()
              import$transform()
              res <- evaluate_promise(fn(import))
              expect_false(res$result)
              mockery::expect_called(mock_no_answer, 1)
              expect_equal(res$messages, "Not uploading to database.\n")
            })

  with_mock("dettl:::dettl_config" = mock_confim_config,
            "askYesNo" = mock_NA_answer, {
              import <- dettl(file.path(path, "example/"), db_name = "test")
              import$extract()
              import$transform()
              res <- evaluate_promise(fn(import))
              expect_false(res$result)
              mockery::expect_called(mock_NA_answer, 1)
              expect_equal(res$messages, "Not uploading to database.\n")
            })

  res <- with_mock("dettl:::dettl_config" = mock_confim_config,
                   "askYesNo" = mock_yes_answer, {
                     import <- dettl(file.path(path, "example/"), db_name = "test")
                     import$extract()
                     import$transform()
                     res <- evaluate_promise(fn(import))
                     expect_true(res$result)
                     mockery::expect_called(mock_yes_answer, 1)
                     expect_match(res$messages, "Running load .*example", all = FALSE)
                   })
})

test_that("run import doesn't ask to confirm run if not configured", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## Mock dettl_config return
  config <- dettl_config(file.path(path, "example/"))
  config$db[["test"]]$confirm <- FALSE
  mock_no_confirm_config <- mockery::mock(config, cycle = TRUE)

  with_mock("dettl:::dettl_config" = mock_no_confirm_config, {
    import <- dettl(file.path(path, "example/"), db_name = "test")
    import$extract()
    import$transform()
    expect_message(import$load(), "Running load .*example")
  })
})

test_that("extract can be run from path", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl_run(file.path(path, "example/"),
                      db_name = "test", stage = "extract")

  expected_data <- data_frame(c("Alice", "Bob", "Clive"),
                              c(25, 43, 76),
                              c(175, 187, 163))
  colnames(expected_data) <- c("name", "age", "height")

  expect_equal(length(import$data$extract), 1)
  expect_equal(import$data$extract$people, expected_data)

  ## There is no transformed data
  expect_equal(import$data$transform, NULL)

  ## Can run on returned import object
  import <- dettl_run(import, stage = "transform")

  ## Extracted data is unchanged
  expect_equal(length(import$data$extract), 1)
  expect_equal(import$data$extract$people, expected_data)

  ## Trasformed data exists
  expected_transform_data <- data_frame(c("Alice", "Bob"),
                                        c(25, 43),
                                        c(175, 187))
  colnames(expected_transform_data) <- c("name", "age", "height")

  expect_equal(length(import$data$transform), 1)
  expect_equal(import$data$transform$people, expected_transform_data)

  ## Data can be loaded
  dettl_run(import, stage = "load")

  expected_load_data <- data_frame(c("Alice", "Bob"),
                                   c(25, 43),
                                   c(175, 187))
  colnames(expected_load_data) <- c("name", "age", "height")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  expect_equal(DBI::dbGetQuery(con, "SELECT name, age, height from people"),
               expected_load_data)
})

test_that("load can be run in one call", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  dettl_run(file.path(path, "example/"), db_name = "test",
            stage = c("extract", "transform", "load"))

  expected_data <- data_frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187))
  colnames(expected_data) <- c("name", "age", "height")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  expect_equal(DBI::dbGetQuery(con, "SELECT name, age, height from people"),
               expected_data)
})

test_that("calling to run transform when extract hasn't been done will run both", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl_run(file.path(path, "example/"), db_name = "test",
                      stage = "transform")

  ## Extracted data has been generated
  expected_data <- data_frame(c("Alice", "Bob", "Clive"),
                              c(25, 43, 76),
                              c(175, 187, 163))
  colnames(expected_data) <- c("name", "age", "height")
  expect_equal(length(import$data$extract), 1)
  expect_equal(import$data$extract$people, expected_data)

  ## Trasformed data exists
  expected_transform_data <- data_frame(c("Alice", "Bob"),
                                        c(25, 43),
                                        c(175, 187))
  colnames(expected_transform_data) <- c("name", "age", "height")

  expect_equal(length(import$data$transform), 1)
  expect_equal(import$data$transform$people, expected_transform_data)
})

test_that("calling to run load when transform not complete returns error", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  expect_error(
    dettl_run(file.path(path, "example/"), db_name = "test", stage = "load"),
    "Can't run load as transform stage has not been run.")

})

test_that("dettl_run can save data", {
  skip_if_not_installed("readxl")
  path <- prepare_test_import("example_automatic_load",
                              add_data = TRUE, add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  tmp <- tempfile()
  mock_tempfile <- mockery::mock(tmp)
  with_mock("tempfile" = mock_tempfile, {
    import <- dettl_run(file.path(path, "example_automatic_load/"),
                        db_name = "test", stage = "extract",
                        save = TRUE)
  })

  expect_equal(readxl::excel_sheets(tmp),
               c("people", "jobs"))

  extr_people <- readxl::read_excel(tmp, sheet = "people")
  expect_equal(colnames(extr_people), c("id", "name", "age", "height"))
  expect_equal(nrow(extr_people), 3)
  extr_jobs <- readxl::read_excel(tmp, sheet = "jobs")
  expect_equal(colnames(extr_jobs), c("person", "job"))
  expect_equal(nrow(extr_jobs), 3)

  ## Can run on returned import object
  save_file <- tempfile(fileext = ".xlsx")
  import <- dettl_run(import, stage = "transform", save = save_file)

  ## Query save file
  expect_equal(readxl::excel_sheets(save_file),
               c("people", "jobs"))

  trans_people <- readxl::read_excel(save_file, sheet = "people")
  expect_equal(colnames(trans_people), c("id", "name", "age", "height"))
  expect_equal(nrow(trans_people), 2)
  trans_jobs <- readxl::read_excel(save_file, sheet = "jobs")
  expect_equal(colnames(trans_jobs), c("person", "job"))
  expect_equal(nrow(trans_jobs), 2)
})

test_that("running extract and transform with save all outputs", {
  skip_if_not_installed("readxl")
  path <- prepare_test_import("example_automatic_load",
                              add_data = TRUE, add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  save_file <- tempfile(fileext = ".xlsx")
  import <- dettl_run(file.path(path, "example_automatic_load/"),
                      db_name = "test", stage = c("extract", "transform"),
                      save = save_file)

  # Query save file
  expect_equal(readxl::excel_sheets(save_file),
               c("extracted_people", "extracted_jobs",
                 "transformed_people", "transformed_jobs"))

  extr_people <- readxl::read_excel(save_file, sheet = "extracted_people")
  expect_equal(colnames(extr_people), c("id", "name", "age", "height"))
  expect_equal(nrow(extr_people), 3)
  extr_jobs <- readxl::read_excel(save_file, sheet = "extracted_jobs")
  expect_equal(colnames(extr_jobs), c("person", "job"))
  expect_equal(nrow(extr_jobs), 3)

  trans_people <- readxl::read_excel(save_file, sheet = "transformed_people")
  expect_equal(colnames(trans_people), c("id", "name", "age", "height"))
  expect_equal(nrow(trans_people), 2)
  trans_jobs <- readxl::read_excel(save_file, sheet = "transformed_jobs")
  expect_equal(colnames(trans_jobs), c("person", "job"))
  expect_equal(nrow(trans_jobs), 2)
})
