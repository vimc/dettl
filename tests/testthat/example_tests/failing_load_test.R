context("failing-test")

testthat::test_that("No of rows in people increases by 1", {
  expect_equal(after$count, before$count + 1)
})
