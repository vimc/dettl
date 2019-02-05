context("failing-transform-test")

testthat::test_that("No of rows in test_data is 1", {
  expect_equal(nrow(transformed_data$test_data), 1)
})
