context("test-runner")

testthat::test_that("user specified load tests can be run", {
  test_path <- "example_tests/"
  test_file <- "failing_load_test.R"
  before <- list(count = 0)
  after <- list(count = 2)
  result <- run_load_tests(test_path, test_file, before, after,
                           SilentReporter)

  expect_false(all_passed(result))

  test_path <- "example_tests/"
  test_file <- "passing_load_test.R"
  result <- run_load_tests(test_path, test_file, before, after,
                           SilentReporter)

  expect_true(all_passed(result))
})

testthat::test_that("user specified extract tests can be run", {
  test_path <- "example_tests/"
  test_file <- "failing_extract_test.R"
  extracted_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_extract_tests(test_path, test_file, extracted_data, NULL,
                              SilentReporter)

  expect_false(all_passed(result))

  test_path <- "example_tests/"
  test_file <- "passing_extract_test.R"
  result <- run_extract_tests(test_path, test_file, extracted_data, NULL,
                              SilentReporter)

  expect_true(all_passed(result))
})

testthat::test_that("user specified transform tests can be run", {
  test_path <- "example_tests/"
  test_file <- "failing_transform_test.R"
  transformed_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_transform_tests(test_path, test_file, transformed_data, NULL,
                                SilentReporter)

  expect_false(all_passed(result))

  test_path <- "example_tests/"
  test_file <- "passing_transform_test.R"
  result <- run_transform_tests(test_path, test_file, transformed_data, NULL,
                                SilentReporter)

  expect_true(all_passed(result))
})

testthat::test_that("connection is available to tests", {
  test_path <- "example_tests/"
  con <- get_local_connection()
  on.exit(DBI::dbDisconnect(con))
  test_file <- "connection_extract_test.R"
  extracted_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_extract_tests(test_path, test_file, extracted_data, con,
                              SilentReporter)
  expect_true(all_passed(result))

  test_file <- "connection_transform_test.R"
  transformed_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_transform_tests(test_path, test_file, transformed_data, con,
                                SilentReporter)
  expect_true(all_passed(result))

  test_path <- "example_tests/"
  test_file <- "connection_load_test.R"
  before <- list(count = 0)
  after <- list(count = 2)
  result <- run_load_tests(test_path, test_file, before, after, con,
                           SilentReporter)
  expect_true(all_passed(result))

})
