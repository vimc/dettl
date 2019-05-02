context("log-import")

test_that("log data is persisted", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  log_data <- get_log_data(sqlite_con, "log_table", file.path(path, "example"),
                           "test comment")
  log_import(sqlite_con, "log_table", log_data)
  sqlite_data <- DBI::dbGetQuery(sqlite_con, "SELECT * FROM log_table")
  expect_true(nrow(sqlite_data) == 1)
  expect_equal(sqlite_data$name, "example")
  ## We want to check logged time is within some reasonable range.
  ## Arbitrarily choose 1 min ago.
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(sqlite_data$date))
  expect_true(as.numeric(sqlite_data$date) < as.numeric(Sys.time()))
  expect_equal(sqlite_data$comment, "test comment")
  expect_equal(sqlite_data$git_user, "dettl")
  expect_equal(sqlite_data$git_email, "email@example.com")
  expect_equal(sqlite_data$git_hash, git_hash(path))
  expect_equal(sqlite_data$git_branch, "master")
})

test_that("postgres log data is persisted", {
  path <- prepare_test_import(create_db = FALSE)
  postgres_con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(postgres_con), add = TRUE)

  log_data <- get_log_data(postgres_con, "log_table", file.path(path, "example"),
                           "test comment")
  log_import(postgres_con, "log_table", log_data)
  postgres_data <- DBI::dbGetQuery(postgres_con, "SELECT * FROM log_table")
  expect_true(nrow(postgres_data) == 1)
  expect_equal(postgres_data$name, "example")
  ## We want to check logged time is within some reasonable range.
  ## Arbitrarily choose 1 min ago.
  expect_true(as.numeric(Sys.time() - 60) < as.numeric(postgres_data$date))
  expect_true(as.numeric(postgres_data$date) < as.numeric(Sys.time()))
  expect_equal(postgres_data$comment, "test comment")
  expect_equal(postgres_data$git_user, "dettl")
  expect_equal(postgres_data$git_email, "email@example.com")
  expect_equal(postgres_data$git_hash, git_hash(path))
  expect_equal(postgres_data$git_branch, "master")
})

test_that("sqlite and postgres dates agree", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  postgres_con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(postgres_con), add = TRUE)

  log_data <- get_log_data(sqlite_con, "log_table", file.path(path, "example"),
                           "test comment")
  log_import(sqlite_con, "log_table", log_data)
  log_import(postgres_con, "log_table", log_data)

  sl_date <- DBI::dbGetQuery(sqlite_con, "SELECT date FROM log_table")[1, ]
  pg_date <- DBI::dbGetQuery(postgres_con, "SELECT date FROM log_table")[1, ]
  expect_type(sl_date, "double")
  expect_type(pg_date, "double")
  expect_s3_class(pg_date, "POSIXct")
  ## Compare dates as UTC POSIXct
  attr(pg_date, "tzone") <- "UTC"
  sl_utc <- as.POSIXct(sl_date, origin = "1970-01-01", tz = "UTC")
  expect_true(pg_date - 1 < sl_utc)
  expect_true(sl_utc < pg_date + 1)
})
