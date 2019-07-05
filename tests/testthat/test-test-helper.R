context("test-helper")

test_that("test helper can set up db for testing", {
  dir <- temp_dir()
  path <- prepare_example_db(dir)

  con <- dbi_db_connect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  expect_true(DBI::dbIsValid(con))
  expect_true("people" %in% DBI::dbListTables(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  data = data_frame(c("Alice", "Bob"),
                                c(25, 43),
                                c(175, 187))
  names(data) <- c("name", "age", "height")
  DBI::dbWriteTable(con, "people", data, append = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 2)
  DBI::dbDisconnect(con)

  ## Calling prepare_example_db recreats the database
  path <- prepare_example_db(dir)
  con <- dbi_db_connect(RSQLite::SQLite(), path)
  expect_true(DBI::dbIsValid(con))
  expect_true("people" %in% DBI::dbListTables(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)
})
