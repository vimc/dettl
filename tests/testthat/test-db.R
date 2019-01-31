context("test-db")

test_that("db can connect to database using yaml config", {

  db_name <- "test.sqlite"
  create_test_db(db_name)
  on.exit(unlink(db_name))

  con <- db_connect("destination", ".")
  expect_true(DBI::dbIsValid(con))

})
