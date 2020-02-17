context("log-import")

test_that("log data is persisted", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  log_data <- build_log_data(file.path(path, "example"), "test comment")
  write_log(sqlite_con, "dettl_import_log", log_data)
  sqlite_data <- DBI::dbGetQuery(sqlite_con, "SELECT * FROM dettl_import_log")
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

  log_data <- build_log_data(file.path(path, "example"), "test comment")
  write_log(postgres_con, "dettl_import_log", log_data)
  postgres_data <- DBI::dbGetQuery(postgres_con,
                                   "SELECT * FROM dettl_import_log")
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

test_that("sqlite and postgres dates can be parsed and agree", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  postgres_con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(postgres_con), add = TRUE)

  log_data <- build_log_data(file.path(path, "example"), "test comment")
  write_log(sqlite_con, "dettl_import_log", log_data)
  write_log(postgres_con, "dettl_import_log", log_data)

  sl_date <- DBI::dbGetQuery(sqlite_con,
                             "SELECT date FROM dettl_import_log")[1, ]
  pg_date <- DBI::dbGetQuery(postgres_con,
                             "SELECT date FROM dettl_import_log")[1, ]
  expect_type(sl_date, "double")
  expect_type(pg_date, "double")
  expect_s3_class(pg_date, "POSIXct")
  ## Compare dates as UTC POSIXct
  attr(pg_date, "tzone") <- "UTC"
  sl_utc <- as.POSIXct(sl_date, origin = "1970-01-01", tz = "UTC")
  expect_true(pg_date - 1 < sl_utc)
  expect_true(sl_utc < pg_date + 1)

  ## Test date parsing
  parsed_date <- parse_sql_date(sqlite_con, sl_date)
  expect_true(pg_date - 1 < parsed_date)
  expect_true(parsed_date < pg_date + 1)
})

test_that("a NULL comment can be persisted", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  postgres_con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(postgres_con), add = TRUE)

  log_data <- build_log_data(file.path(path, "example"), NULL)
  write_log(sqlite_con, "dettl_import_log", log_data)
  write_log(postgres_con, "dettl_import_log", log_data)

  sl <- DBI::dbGetQuery(sqlite_con, "SELECT comment FROM dettl_import_log")[1, ]
  pg <- DBI::dbGetQuery(postgres_con,
                        "SELECT comment FROM dettl_import_log")[1, ]
  expect_true(is.na(sl))
  expect_true(is.na(pg))
})
