context("connection-transform-test")

testthat::test_that("valid connection is available", {
  expect_true(DBI::dbIsValid(con))
})
