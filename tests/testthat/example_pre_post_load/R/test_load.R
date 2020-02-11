context("counts-test")

testthat::test_that("No of rows in people increases by 2", {
  expect_equal(after$people_count, before$people_count + 3)
  expect_equal(after$jobs_count, before$jobs_count + 2)
})

testthat::test_that("index has been created", {
  indexes <- DBI::dbGetQuery(con, "PRAGMA  index_list('people')")
  expect_equal("people_name" %in% indexes$name)
})
