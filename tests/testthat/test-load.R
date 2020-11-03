context("load")

testthat::test_that("messages are printed to console when tests are run", {
  path <- prepare_test_import("example")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example"), db_name = "test")
  import$extract()
  import$transform()

  ## Ideally here we would run run_load and check for messages using
  ## expect_message. This doesn't support checking for 2 messages from one call
  ## and calling run_load twice violates the unique key constraint so work
  ## around this by storing the messages in a variable and checking these
  ## individually.
  run_load_call <- function() {
    import$load()
  }
  res <- evaluate_promise(run_load_call())
  expect_true(any(grepl(sprintf(
    "Running load tests R/test_load.R"),
    fixed = TRUE, res$messages)))
  expect_true(any(grepl("All tests passed, commiting changes to database.",
                        res$messages, fixed = TRUE)))
  expect_true(any(grepl("Running load in a transaction:",
                        res$messages, fixed = TRUE)))
})

testthat::test_that("log table is appended to", {
  path <- prepare_test_import("example")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example"), db_name = "test")
  import$extract()
  import$transform()
  import$load(comment = "Test comment")

  con <- import$get_connection()
  log_data <- DBI::dbGetQuery(con, "SELECT * FROM dettl_import_log")
  expect_true(nrow(log_data) == 1)
  expect_equal(log_data$name, "example")
  ## We want to check logged time is within some reasonable range.
  ## Arbitrarily choose 1 min ago.
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(log_data$start_time))
  expect_true(as.numeric(log_data$start_time) < as.numeric(Sys.time()))
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(log_data$end_time))
  expect_true(as.numeric(log_data$end_time) < as.numeric(Sys.time()))
  duration <- as.numeric(log_data$end_time) - as.numeric(log_data$start_time)
  ## Duration includes extract + transform time so check that it is not
  ## identical but is within a few ms either side
  load_duration <- round(duration, digits = 3)
  expect_true(load_duration != log_data$duration)
  expect_true(load_duration - 0.1 < log_data$duration)
  expect_true(load_duration + 0.1 > log_data$duration)
  expect_equal(log_data$comment, "Test comment")
  expect_equal(log_data$git_user, "dettl")
  expect_equal(log_data$git_email, "email@example.com")
  expect_equal(log_data$git_hash, git_hash(path))
  expect_equal(log_data$git_branch, "master")
})

testthat::test_that("postgres log table is appended to", {
  path <- build_git_demo("example", "dettl_config.yml")
  con <- prepare_example_postgres_db()

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example"), db_name = "psql_test")
  import$extract()
  import$transform()
  import$load(comment = "Test comment")

  log_data <- DBI::dbGetQuery(con, "SELECT * FROM dettl_import_log")
  expect_true(nrow(log_data) == 1)
  expect_equal(log_data$name, "example")
  ## We want to check logged time is within some reasonable range.
  ## Arbitrarily choose 1 min ago.
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(log_data$start_time))
  expect_true(as.numeric(log_data$start_time) < as.numeric(Sys.time()))
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(log_data$end_time))
  expect_true(as.numeric(log_data$end_time) < as.numeric(Sys.time()))
  duration <- as.numeric(log_data$end_time) - as.numeric(log_data$start_time)
  ## Duration includes extract + transform time so check that it is not
  ## identical but is within a few ms either side
  load_duration <- round(duration, digits = 3)
  expect_true(load_duration != log_data$duration)
  expect_true(load_duration - 0.1 < log_data$duration)
  expect_true(load_duration + 0.1 > log_data$duration)
  expect_equal(log_data$comment, "Test comment")
  expect_equal(log_data$git_user, "dettl")
  expect_equal(log_data$git_email, "email@example.com")
  expect_equal(log_data$git_hash, git_hash(path))
  expect_equal(log_data$git_branch, "master")
})

test_that("import can only be run once", {
  path <- prepare_test_import("example")

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example"), db_name = "test")
  import$extract()
  import$transform()
  import$load()
  expect_error(import$load(),
               "Import has previously been run. Previous run log:
  name:           example
  start time:     [0-9:\\s-]+
  end time:       [0-9:\\s-]+
  duration:       [0-9.]+
  comment:        NA
  git user.name:  dettl
  git user.email: email@example.com
  git branch:     master
  git hash:       \\w+", perl = TRUE)
})
