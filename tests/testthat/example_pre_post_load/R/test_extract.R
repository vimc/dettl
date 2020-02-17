context("test-extract")

testthat::test_that("extracted data populated correctly", {
  expect_equal(nrow(extracted_data$people), 3)
  expect_equal(nrow(extracted_data$jobs), 3)
})
