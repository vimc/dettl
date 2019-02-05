context("test-runner")

testthat::test_that("user specified load tests can be run", {
  test_path <- "example_tests/"
  test_file <- "failing_load_test.R"
  before <- list(count = 0)
  after <- list(count = 2)
  result <- run_load_tests(test_path, test_file, before, after)

  expect_false(all_passed(result))

  test_path <- "example_tests/"
  test_file <- "passing_load_test.R"
  result <- run_load_tests(test_path, test_file, before, after)

  expect_true(all_passed(result))
})

testthat::test_that("user specified extract tests can be run", {
  test_path <- "example_tests/"
  test_file <- "failing_extract_test.R"
  extracted_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_extract_tests(test_path, test_file, extracted_data)

  expect_false(all_passed(result))

  test_path <- "example_tests/"
  test_file <- "passing_extract_test.R"
  result <- run_extract_tests(test_path, test_file, extracted_data)

  expect_true(all_passed(result))
})

testthat::test_that("user specified transform tests can be run", {
  test_path <- "example_tests/"
  test_file <- "failing_transform_test.R"
  transformed_data <- list("test_data" = data.frame(c(1,2), c(3,4)))
  result <- run_transform_tests(test_path, test_file, transformed_data)

  expect_false(all_passed(result))

  test_path <- "example_tests/"
  test_file <- "passing_transform_test.R"
  result <- run_transform_tests(test_path, test_file, transformed_data)

  expect_true(all_passed(result))
})
