context("insert-into-returning")

test_that("can add data to SQLite database retuning primary key", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  data <- data.frame(list(
    name = c("Alice", "Bob"),
    age = c(34, 54),
    height = c(176, 189)
  ), stringsAsFactors = FALSE)

  ids <- insert_into_returning(con, "people", data, "id")
  expect_equal(ids, list(1, 2))

  data <- data.frame(list(
    name = c("Clive", "Daisy"),
    age = c(23, 45),
    height = c(193, 185)
  ), stringsAsFactors = FALSE)

  names <- insert_into_returning(con, "people", data, "id", "name")
  expect_equal(names, list("Clive", "Daisy"))

  ## Trying to insert same data returns ID of existing rows
  ids <- insert_into_returning(con, "people", data, c("name", "age", "height"),
                               "id")
  expect_equal(ids, list(3, 4))

  ## Inserting data with null key adds data by default
  data <- data.frame(list(
    name = c("Ed"),
    age = c(98),
    height = c(183)
  ), stringsAsFactors = FALSE)

  ids <- insert_into_returning(con, "people", data)
  expect_equal(ids, list(5))
})

test_that("can add data to Postgres database returning primary key", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))

  ##browser()
  data <- data.frame(list(
    name = c("Alice", "Bob"),
    age = c(34, 54),
    height = c(176, 189)
  ), stringsAsFactors = FALSE)

  ids <- insert_into_returning(con, "people", data, "id")
  expect_equal(ids, list(1, 2))

  data <- data.frame(list(
    name = c("Clive", "Daisy"),
    age = c(23, 45),
    height = c(193, 185)
  ), stringsAsFactors = FALSE)

  names <- insert_into_returning(con, "people", data, "id", "name")
  expect_equal(names, list("Clive", "Daisy"))

  ## Trying to insert same data returns ID of existing rows
  ids <- insert_into_returning(con, "people", data, c("name", "age", "height"),
                               "id")
  expect_equal(ids, list(3, 4))

  ## Inserting data with null key adds data by default
  data <- data.frame(list(
    name = c("Ed"),
    age = c(98),
    height = c(183)
  ), stringsAsFactors = FALSE)

  ids <- insert_into_returning(con, "people", data)
  expect_equal(ids, list(5))
})
