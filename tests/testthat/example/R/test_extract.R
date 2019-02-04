context("test-extract")

testthat::test_file("extracted data contains 3 rows", {
  expect_nrow(nrow(extracted_data), 3)
})
