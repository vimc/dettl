context("test-runner")

testthat::test_that("user specified tests can be run", {
  test_path <- "example_tests/failing_test.R"
  before <- list(count = 0)
  after <- list(count = 2)
  result <- run_tests(test_path, before, after)

  expect_false(all_passed(result))

  test_path <- "example_tests/passing_test.R"
  result <- run_tests(test_path, before, after)

  expect_true(all_passed(result))
})
