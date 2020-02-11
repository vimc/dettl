context("counts-test")

testthat::test_that("hobbies table created", {
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from hobbies")[1,1], 3)
})

testthat::test_that("houses table created", {
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from abode")[1,1], 2)
})
