context("dettl-config")

test_that("dettl config can be read and database connection info extracted", {
  path <- setup_config()
  cfg <- db_config(path)
  expect_s3_class(cfg, "db_config")

  expect_equal(cfg$db$example$driver, c("RSQLite", "SQLite"))
  expect_equal(cfg$db$example$args, list(dbname = "test.sqlite"))

  expect_equal(cfg$db$uat$driver, c("RPostgres", "Postgres"))
  expect_equal(cfg$db$uat$args, list(
    dbname = "montagu",
    host = "https://example.com",
    port = 12345,
    user = "readonly",
    password = "VAULT:/secret/users/readonly:password")
  )
})

test_that("reading config throws error if driver is not configured", {
  path <- setup_config(db_driver = "")
  expect_error(db_config(path), "No driver specified for DB config example.")
})

test_that("error is thrown when db config is missing", {
  expect_error(dettl_locate_config(".."),
               "Reached root from .. without finding 'db_config.yml'")
})

test_that("vault server details can be read from db config", {
  path <- setup_config(vault_server = "")
  cfg <- db_config(path)
  expect_null(cfg$vault_server)

  path <- setup_config()
  cfg <- db_config(path)
  expect_equal(cfg$vault_server, "https://example.com")

  path <- setup_config(vault_server = 234)
  expect_error(db_config(path), "'.+:vault_server' must be character")
})

test_that("read config loads config from directory", {

  cfg <- read_config("example")
  expect_s3_class(cfg, "dettl_config")

  expect_length(cfg, 7)

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

  expect_true("rewrite_keys" %in% names(cfg))
  expect_equal(class(cfg$rewrite_keys), c("ForeignKeyConstraints", "R6"))

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

  expect_false("rewrite_keys" %in% names(cfg))
})

test_that("read config fails if required configuration is not available", {
  expect_error(read_config("broken_example"),
    "No files found matching file pattern script.R")
})

test_that("wildcards in sources are expanded", {
  sources <- "R/*.R"
  files <- expand_wildcards(sources, "example")
  expect_equal(files, normalizePath(c("example/R/extract.R", "example/R/load.R",
                        "example/R/test_extract.R", "example/R/test_load.R",
                        "example/R/test_transform.R", "example/R/transform.R",
                        "example/R/verification_queries.R")))

  sources <- c("example/R/extract.R", "example/R/load.R",
               "example/R/transform.R")
  files <- expand_wildcards(sources, ".")
  expect_length(files, 3)

  sources <- "no_match.R"
  expect_error(expand_wildcards(sources, "."),
                 "No files found matching file pattern no_match.R")
})

test_that("read config can be called with default load", {
  cfg <- read_config("simple_example", default_load = TRUE)

  expect_true("load" %in% names(cfg))
  expect_length(cfg$load, 2)
  expect_false("func" %in% names(cfg$load))
  expect_true("test" %in% names(cfg$load))
  expect_true("verification_queries" %in% names(cfg$load))
  expect_is(cfg$load$verification_queries, "function")
  expect_equal(cfg$load$test, "R/test_load.R")
})
