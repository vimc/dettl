context("test-db")

test_that("db can connect to database using yaml config", {

  db_name <- "test.sqlite"
  prepare_example_db(db_name)
  on.exit(unlink(db_name))

  con <- db_connect("destination", ".")
  expect_true(DBI::dbIsValid(con))
})

test_that("dettl can connect to remote DB using yaml config", {
  con <- db_connect("source", "uat_config")
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbIsValid(con))
  expect_true(length(DBI::dbListTables(con)) > 0)
})
