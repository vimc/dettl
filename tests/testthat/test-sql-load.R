context("sql-load")

test_that("can run sql from file", {
  t <- tempfile()
  writeLines(c(
    "/* a multiline sql",
    "comment",
    "for testing */",
    "create table test (",
    "-- another comment",
    "  id INTEGER -- more comments",
    ")"
  ), t)

  sql_load <- get_sql_load(t)
  expect_type(sql_load, "closure")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  sql_load(con)
  expect_equal(DBI::dbListTables(con), "test")
})

test_that("useful error returned when sql fails to run", {
  t <- tempfile()
  writeLines(c(
    "some bad sql"
  ), t)

  sql_load <- get_sql_load(t)
  expect_type(sql_load, "closure")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_error(sql_load(con),
               "SQL error:\nnear \"some\": syntax error")
})

test_that("sql load can run multiple statements", {
  t <- tempfile()
  writeLines(c(
    "create table test (",
    "  id INTEGERs",
    ");",
    "create table test2 (",
    "  id INTEGERs",
    ");",
    " "
  ), t)

  sql_load <- get_sql_load(t)
  expect_type(sql_load, "closure")
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  expect_warning(sql_load(con), NA)
  expect_equal(DBI::dbListTables(con), c("test", "test2"))
})
