context("load")

testthat::test_that("people has 2 rows 2", {
  #expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 2)
  expect_equal(after$count, before$count + 2)
})
