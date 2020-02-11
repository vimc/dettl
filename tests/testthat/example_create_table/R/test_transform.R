context("test-transform")

testthat::test_that("transformed data contains correct no of rows", {
  expect_equal(nrow(transformed_data$hobbies), 3)
  expect_equal(nrow(transformed_data$abode), 2)
})
