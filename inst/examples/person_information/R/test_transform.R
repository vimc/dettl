context("transform")

testthat::test_that("transformed data contains 2 rows", {
  expect_equal(nrow(transformed_data$people), 2)
})
