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
               "Cannot run load as no data has been transformed.")
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

  ## Import can skip git checks using force
  import_load <- import$load(force = TRUE)
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

  extracted_data <- dettl_run_extract(file.path(path, "example/"),
                                      db_name = "test")

  expected_data <- data_frame(c("Alice", "Bob", "Clive"),
                              c(25, 43, 76),
                              c(175, 187, 163))
  colnames(expected_data) <- c("name", "age", "height")

  expect_equal(length(extracted_data), 1)
  expect_equal(extracted_data$people, expected_data)
})

test_that("transform can be run from path", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  transformed_data <- dettl_run_transform(file.path(path, "example/"),
                                      db_name = "test")

  expected_data <- data_frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187))
  colnames(expected_data) <- c("name", "age", "height")

  expect_equal(length(transformed_data), 1)
  expect_equal(transformed_data$people, expected_data)
})

test_that("extract can be run from path", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  dettl_run_load(file.path(path, "example/"), db_name = "test")

  expected_data <- data_frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187))
  colnames(expected_data) <- c("name", "age", "height")

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  expect_equal(DBI::dbGetQuery(con, "SELECT name, age, height from people"),
               expected_data)
})

test_that("require_branch prevents branch changes if configured", {
  path <- prepare_test_import()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  ## Mock dettl_config return
  cfg <- file.path(path, "dettl_config.yml")
  dat <- yaml_read(file.path(path, "dettl_config.yml"))
  dat$db$test$require_branch <- "deploy"
  yaml::write_yaml(dat, cfg)

  expect_error(
    dettl_run_load(file.path(path, "example/"), db_name = "test"),
    "This import can only be run from the 'deploy' branch")

  gert::git_branch_create("deploy", repo = path)
  gert::git_branch_checkout("deploy", repo = path)

  dettl_run_load(file.path(path, "example/"), db_name = "test", force = TRUE)

  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- DBI::dbReadTable(con, "dettl_import_log")
  expect_equal(res$name, "example")
  expect_equal(res$git_branch, "deploy")
})

test_that("can get extracted data if tests fail", {
  path <- prepare_test_import("example_failing_extract_test")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_failing_extract_test/"))
  expect_error(import$extract(),
               "Not all extract tests passed. Fix tests before proceeding.")

  extracted_data <- import$get_extracted_data()
  expect_true(!is.null(extracted_data))

  ## Trying to run transform after failed extract test throws error
  expect_error(import$transform(),
               "Cannot run transform as extract tests failed.")
})

test_that("can get transformed data if tests fail", {
  path <- prepare_test_import("example_failing_transform_test")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_failing_transform_test/"))
  import$extract()
  expect_error(import$transform(),
               "Not all transform tests passed. Fix tests before proceeding.")

  transformed_data <- import$get_transformed_data()
  expect_true(!is.null(transformed_data))

  ## Trying to run load after failed transform tests throws error
  expect_error(import$load(), "Cannot run load as transform tests failed.")
})

test_that("re-running extract invalidates transformed data", {
  path <- prepare_test_import("example")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example/"))
  import$extract()
  import$transform()

  expect_false(is.null(import$get_transformed_data()))

  import$extract()
  expect_true(is.null(import$get_transformed_data()))

  ## Reloading invalidates all data
  import$transform()
  expect_false(is.null(import$get_extracted_data()))
  expect_false(is.null(import$get_transformed_data()))

  import$reload()
  expect_true(is.null(import$get_extracted_data()))
  expect_true(is.null(import$get_transformed_data()))
})
