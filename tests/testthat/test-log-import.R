context("log-import")

test_that("log data is persisted", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  log <- ImportLog$new(sqlite_con, "dettl_import_log",
                       file.path(path, "example"), "r", "append")
  log$set_comment("test comment")
  log$write_log()
  sqlite_data <- DBI::dbGetQuery(sqlite_con, "SELECT * FROM dettl_import_log")
  expect_true(nrow(sqlite_data) == 1)
  expect_equal(sqlite_data$name, "example")
  expect_equal(sqlite_data$language, "r")
  expect_equal(sqlite_data$mode, "append")
  expect_equal(sqlite_data$start_time, NA_real_)
  expect_equal(sqlite_data$end_time, NA_real_)
  expect_equal(sqlite_data$duration, NA_real_)
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

  log <- ImportLog$new(postgres_con, "dettl_import_log",
                       file.path(path, "example"), "r", "append")
  log$set_comment("test comment")
  log$write_log()
  postgres_data <- DBI::dbGetQuery(postgres_con,
                                   "SELECT * FROM dettl_import_log")
  expect_true(nrow(postgres_data) == 1)
  expect_equal(postgres_data$name, "example")
  expect_equal(postgres_data$language, "r")
  expect_equal(postgres_data$mode, "append")
  expect_equal(postgres_data$start_time, as.POSIXct(NA))
  expect_equal(postgres_data$end_time, as.POSIXct(NA))
  expect_equal(postgres_data$duration, NA_real_)
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

  sqlite_log <- ImportLog$new(sqlite_con, "dettl_import_log",
                              file.path(path, "example"), "r", "append")
  sqlite_log$set_comment("test comment")
  postgres_log <- ImportLog$new(postgres_con, "dettl_import_log",
                                file.path(path, "example"), "r", "append")
  postgres_log$set_comment("test comment")
  sqlite_log$start_timer()
  postgres_log$start_timer()
  sqlite_log$write_log()
  postgres_log$write_log()

  sl_start <- DBI::dbGetQuery(sqlite_con,
                             "SELECT start_time FROM dettl_import_log")[1, ]
  pg_start <- DBI::dbGetQuery(postgres_con,
                             "SELECT start_time FROM dettl_import_log")[1, ]
  expect_type(sl_start, "double")
  expect_type(pg_start, "double")
  expect_s3_class(pg_start, "POSIXct")
  ## Compare dates as UTC POSIXct
  attr(pg_start, "tzone") <- "UTC"
  sl_utc <- as.POSIXct(sl_start, origin = "1970-01-01", tz = "UTC")
  expect_true(pg_start - 1 < sl_start)
  expect_true(sl_utc < pg_start + 1)

  ## Test date parsing
  parsed_date <- parse_sql_date(sqlite_con, sl_start)
  expect_true(pg_start - 1 < parsed_date)
  expect_true(parsed_date < pg_start + 1)
})

test_that("a NULL comment can be persisted", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  postgres_con <- prepare_example_postgres_db()
  on.exit(DBI::dbDisconnect(postgres_con), add = TRUE)

  sqlite_log <- ImportLog$new(sqlite_con, "dettl_import_log",
                              file.path(path, "example"), "r", "append")
  postgres_log <- ImportLog$new(postgres_con, "dettl_import_log",
                                file.path(path, "example"), "r", "append")
  sqlite_log$write_log()
  postgres_log$write_log()

  sl <- DBI::dbGetQuery(sqlite_con, "SELECT comment FROM dettl_import_log")[1, ]
  pg <- DBI::dbGetQuery(postgres_con,
                        "SELECT comment FROM dettl_import_log")[1, ]
  expect_true(is.na(sl))
  expect_true(is.na(pg))
})

test_that("timer can be stopped and restarted", {
  path <- prepare_test_import()
  con <- db_connect("test", path)
  log <- ImportLog$new(con, "dettl_import_log", file.path(path, "example"),
                       "r", "append")
  log$start_timer()
  Sys.sleep(1)
  log$stop_timer()
  start <- log$log_data$start_time
  end <- log$log_data$end_time
  duration <- log$log_data$duration
  expect_true(!is.null(start))
  expect_true(!is.null(end))
  expect_true(duration >= 1)
  expect_true(duration < 2)

  log$start_timer()
  Sys.sleep(1)
  log$stop_timer()
  ## Start time keeps the initial start time
  expect_equal(log$log_data$start_time, start)
  ## End time gets updated
  expect_true(log$log_data$end_time != end)
  ## Duration is sum of separate times
  new_duration <- log$log_data$duration
  expect_true(new_duration >= 2)
})

test_that("null mode log data can be", {
  path <- prepare_test_import()
  sqlite_con <- db_connect("test", path)
  on.exit(DBI::dbDisconnect(sqlite_con), add = TRUE)

  log <- ImportLog$new(sqlite_con, "dettl_import_log",
                       file.path(path, "example"), "sql", NULL)
  log$set_comment("test comment")
  log$write_log()
  sqlite_data <- DBI::dbGetQuery(sqlite_con, "SELECT * FROM dettl_import_log")
  expect_true(nrow(sqlite_data) == 1)
  expect_equal(sqlite_data$name, "example")
  expect_equal(sqlite_data$language, "sql")
  expect_equal(sqlite_data$mode, NA_character_)
})
