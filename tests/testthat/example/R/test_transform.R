context("test-transform")

testthat::test_file("transformed data contains 2 rows", {
  expect_nrow(nrow(transformed_data), 2)
})
