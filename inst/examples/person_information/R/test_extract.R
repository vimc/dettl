context("extract")

testthat::test_that("extracted data contains 3 rows", {
  expect_equal(nrow(extracted_data$people), 3)
})
