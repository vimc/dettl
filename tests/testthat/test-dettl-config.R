context("dettl-config")

test_that("dettl config can be read and database connection info extracted", {
  cfg <- db_config("db_config")
  expect_s3_class(cfg, "db_config")

  expect_equal(cfg$db$example$driver, c("RSQLite", "SQLite"))
  expect_equal(cfg$db$example$args, list(dbname = "test.sqlite"))

  expect_equal(cfg$db$uat$driver, c("RPostgres", "Postgres"))
  expect_equal(cfg$db$uat$args, list(
    dbname = "montagu",
    host = "https://example.com",
    port = 12345,
    user = "test",
    password = "test")
  )

  dest_dat <- dettl_db_args("example", cfg)
  expect_identical(dest_dat$driver, RSQLite::SQLite)
  expect_identical(dest_dat$args$dbname, file.path(cfg$path, "test.sqlite"))

  dest_dat <- dettl_db_args("uat", cfg)
  expect_identical(dest_dat$driver, RPostgres::Postgres)
  expect_identical(dest_dat$args$dbname, "montagu")

  expect_error(dettl_db_args("missing", cfg),
               "Cannot find config for database missing.")
})

test_that("error is thrown when db config is missing", {
  expect_error(dettl_locate_config(".."),
               "Reached root from .. without finding 'db_config.yml'")
})

test_that("read config loads config from directory", {
  cfg <- read_config("example")
  expect_s3_class(cfg, "dettl_config")

  expect_length(cfg, 6)

  expect_true("extract" %in% names(cfg))
  expect_length(cfg$extract, 2)
  expect_true("func" %in% names(cfg$extract))
  expect_true("test" %in% names(cfg$extract))
  expect_is(cfg$extract$func, "function")
  expect_equal(cfg$extract$test, "R/test_extract.R")

  expect_true("transform" %in% names(cfg))
  expect_length(cfg$transform, 2)
  expect_true("func" %in% names(cfg$transform))
  expect_true("test" %in% names(cfg$transform))
  expect_is(cfg$transform$func, "function")
  expect_equal(cfg$transform$test, "R/test_transform.R")

  expect_true("load" %in% names(cfg))
  expect_length(cfg$load, 3)
  expect_true("func" %in% names(cfg$load))
  expect_true("test" %in% names(cfg$load))
  expect_true("verification_queries" %in% names(cfg$load))
  expect_is(cfg$load$func, "function")
  expect_equal(cfg$load$test, "R/test_load.R")
  expect_is(cfg$load$verification_queries, "function")

  expect_true("name" %in% names(cfg))
  expect_equal(cfg$name, "example")

  expect_true("path" %in% names(cfg))
  expect_equal(cfg$path, "example")
})

test_that("read config adds missing fields from defaults", {
  cfg <- read_config("simple_example")
  expect_s3_class(cfg, "dettl_config")

  expect_length(cfg, 6)

  expect_true("extract" %in% names(cfg))
  expect_length(cfg$extract, 1)
  expect_true("func" %in% names(cfg$extract))
  expect_is(cfg$extract$func, "function")
  expect_equal(cfg$extract$func(), "Executed extract function")

  expect_true("transform" %in% names(cfg))
  expect_length(cfg$transform, 1)
  expect_true("func" %in% names(cfg$transform))
  expect_is(cfg$transform$func, "function")
  expect_equal(cfg$transform$func(), "Executed transform function")

  expect_true("load" %in% names(cfg))
  expect_length(cfg$load, 3)
  expect_true("func" %in% names(cfg$load))
  expect_true("test" %in% names(cfg$load))
  expect_true("verification_queries" %in% names(cfg$load))
  expect_is(cfg$load$func, "function")
  expect_equal(cfg$load$func(), "Executed load function")
  expect_is(cfg$load$verification_queries, "function")
  expect_equal(cfg$load$test, "R/test_load.R")

  expect_true("name" %in% names(cfg))
  expect_equal(cfg$name, "simple_example")

  expect_true("path" %in% names(cfg))
  expect_equal(cfg$path, "simple_example")
})

test_that("read config fails if required configuration is not available", {
  expect_error(read_config("broken_example"),
               "File does not exist: 'script.R' in directory broken_example")
})
