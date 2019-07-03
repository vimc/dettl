context("insert-into-returning")

test_that("can add data to SQLite database returning primary key", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  data <- data_frame(
    name = c("Alice", "Bob"),
    age = c(34, 54),
    height = c(176, 189))

  ids <- insert_into_returning(con, "people", data, "id")
  expect_equal(ids, data_frame(id = c(1,2)))

  data <- data_frame(
    name = c("Clive", "Daisy"),
    age = c(23, 45),
    height = c(193, 185))

  names <- insert_into_returning(con, "people", data, "id", "name")
  expect_equal(names, data_frame("name" = c("Clive", "Daisy")))

  ## Trying to insert same data returns ID of existing rows
  ids <- insert_into_returning(con, "people", data, c("name", "age", "height"),
                               "id")
  expect_equal(ids, data_frame("id" = c(3, 4)))

  ## Inserting data with null key adds data by default
  data <- data_frame(
    name = c("Ed"),
    age = c(98),
    height = c(183))

  ids <- insert_into_returning(con, "people", data)
  expect_equal(ids, data_frame(id = 5))
})

test_that("data can be added to SQLite and check for existing rows by key", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  data <- data_frame(
    name = c("Alice"),
    age = c(34),
    height = c(176)
  )

  ids <- insert_into_returning(con, "people", data, "name", "id")
  expect_equal(ids, data_frame(id = 1))

  ## Return id of existing row if trying to insert again with same name
  ids <- insert_into_returning(con, "people", data, "name", "id")
  expect_equal(ids, data_frame(id = 1))
})

test_that("data can be added to postgres and check for existing rows by key", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))

  data <- data_frame(
    name = c("Alice"),
    age = c(34),
    height = c(176)
  )

  ids <- insert_into_returning(con, "people", data, "name", "id")
  expect_equal(ids, data_frame(id = 1))

  ## Return id of existing row if trying to insert again with same name
  ids <- insert_into_returning(con, "people", data, "name", "id")
  expect_equal(ids, data_frame(id = 1))
})

test_that("can add data to Postgres database returning primary key", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))

  data <- data_frame(
    name = c("Alice", "Bob"),
    age = c(34, 54),
    height = c(176, 189))

  ids <- insert_into_returning(con, "people", data, "id")
  expect_equal(ids, data_frame(id = c(1, 2)))

  data <- data_frame(
    name = c("Clive", "Daisy"),
    age = c(23, 45),
    height = c(193, 185))

  names <- insert_into_returning(con, "people", data, "id", "name")
  expect_equal(names, data_frame(name = c("Clive", "Daisy")))

  ## Trying to insert same data returns ID of existing rows
  ids <- insert_into_returning(con, "people", data, c("name", "age", "height"),
                               "id")
  expect_equal(ids, data_frame(id = c(3, 4)))

  ## Inserting data with null key adds data by default
  data <- data_frame(
    name = c("Ed"),
    age = c(98),
    height = c(183))

  ids <- insert_into_returning(con, "people", data)
  expect_equal(ids, data_frame(id = 5))
})

test_that("can add data to SQLite database and returning multiple columns", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  data <- data_frame(
    name = c("Alice", "Bob"),
    age = c(34, 54),
    height = c(176, 189))

  ids <- insert_into_returning(con, "people", data, key = "id", c("id", "name"))
  expect_equal(ids, data_frame("id" = c(1,2), "name" = c("Alice", "Bob")))

})

test_that("can add data to Postgres database returning multiple columns", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))

  data <- data_frame(
    name = c("Alice", "Bob"),
    age = c(34, 54),
    height = c(176, 189))

  ids <- insert_into_returning(con, "people", data, key = "id", c("id", "name"))
  expect_equal(ids, data_frame("id" = c(1,2), "name" = c("Alice", "Bob")))
})

test_that("empty row can be inserted into SQLite db", {
  path <- prepare_test_import()
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  data <- as.data.frame(matrix(nrow = 2, ncol = 0))

  ids <- insert_into_returning(con, "people", data, key = "id", "id")
  expect_equal(ids, data_frame("id" = c(1,2)))
})

test_that("empty row can be inserted into Postgres db", {
  con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(con))

  data <- as.data.frame(matrix(nrow = 2, ncol = 0))

  ids <- insert_into_returning(con, "people", data, key = "id", "id")
  expect_equal(ids, data_frame("id" = c(1,2)))
})
