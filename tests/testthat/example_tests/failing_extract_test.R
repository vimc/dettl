context("failing-extract-test")

testthat::test_that("No of rows in test_data is 1", {
  expect_equal(nrow(extracted_data$test_data), 1)
})
