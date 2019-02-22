context("test-helper")

test_that("test helper can set up db for testing", {
  dir <- tempdir()
  path <- prepare_example_db("db.sqlite", dir)

  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbIsValid(con))
  expect_true("people" %in% DBI::dbListTables(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  data = data.frame(c("Alice", "Bob"),
                                c(25, 43),
                                c(175, 187),
                                stringsAsFactors = FALSE)
  names(data) <- c("name", "age", "height")
  DBI::dbWriteTable(con, "people", data, append = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 2)
  DBI::dbDisconnect(con)

  ## Calling prepare_example_db recreats the database
  path <- prepare_example_db("db.sqlite", dir)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  expect_true(DBI::dbIsValid(con))
  expect_true("people" %in% DBI::dbListTables(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)
})
