context("test-runner")

testthat::test_that("user specified load tests can be run", {
  test_path <- "example_tests/failing_load_test.R"
  before <- list(count = 0)
  after <- list(count = 2)
  result <- run_load_tests(test_path, before, after, NULL, NULL, NULL, SilentReporter)

  expect_false(all_passed(result))

  test_path <- "example_tests/passing_load_test.R"
  result <- run_load_tests(test_path, before, after, NULL,NULL, NULL, SilentReporter)

  expect_true(all_passed(result))
})

testthat::test_that("user specified extract tests can be run", {
  test_path <- "example_tests/failing_extract_test.R"
  extracted_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_extract_tests(test_path, extracted_data, NULL, SilentReporter)

  expect_false(all_passed(result))

  test_path <- "example_tests/passing_extract_test.R"
  result <- run_extract_tests(test_path, extracted_data, NULL, SilentReporter)

  expect_true(all_passed(result))
})

testthat::test_that("user specified transform tests can be run", {
  test_path <- "example_tests/failing_transform_test.R"
  transformed_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_transform_tests(test_path, extracted_data = NULL, transformed_data, NULL,
                                SilentReporter)

  expect_false(all_passed(result))

  test_path <- "example_tests/passing_transform_test.R"
  result <- run_transform_tests(test_path, extracted_data = NULL, transformed_data, NULL,
                                SilentReporter)

  expect_true(all_passed(result))
})

testthat::test_that("connection is available to tests", {
  test_path <- "example_tests/connection_extract_test.R"
  con <- get_local_connection()
  on.exit(DBI::dbDisconnect(con))
  extracted_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_extract_tests(test_path, extracted_data, con, SilentReporter)
  expect_true(all_passed(result))

  test_path <- "example_tests/connection_transform_test.R"
  transformed_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_transform_tests(test_path, extracted_data = NULL, transformed_data, con,
                                SilentReporter)
  expect_true(all_passed(result))

  test_path <- "example_tests/connection_load_test.R"
  before <- list(count = 0)
  after <- list(count = 2)
  result <- run_load_tests(test_path, before, after, NULL, NULL, con, SilentReporter)
  expect_true(all_passed(result))

})

testthat::test_that("no tests completing counts as passed suite", {
  test_path <- "example_tests/empty_extract_test.R"
  extracted_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_extract_tests(test_path, extracted_data, NULL, SilentReporter)
  expect_true(all_passed(result))
})


test_that("can reload changed sources", {
  path <- prepare_test_import()
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  p <- file.path(path, "example", "R", "extract.R")
  txt <- readLines(p)
  writeLines(txt[!grepl("raw_data$people", txt, fixed = TRUE)], p)

  import <- dettl(file.path(path, "example/"), db_name = "test")
  expect_error(import$extract(),
               "Not all extract tests passed. Fix tests before proceeding")

  writeLines(txt, p)
  import$reload()
  expect_error(import$extract(), NA)
})
