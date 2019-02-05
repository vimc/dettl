context("passing-transform-test")

testthat::test_that("No of rows in test_data is 2", {
  expect_equal(nrow(transformed_data$test_data), 2)
})
