context("test-transform")

testthat::test_that("transformed data contains expected data", {
  expect_equal(nrow(transformed_data$people), 2)
  expect_equal(nrow(transformed_data$jobs), 2)
})
