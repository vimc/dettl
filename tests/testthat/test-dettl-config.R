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

test_that("database log table uses default when not configured", {
  cfg <- db_config(".")
  expect_equal(cfg$db$test$log_table, "import_log")
})

test_that("database log table can be configured", {
  path <- setup_config()
  cfg <- db_config(path)

  expect_equal(cfg$db$example$log_table, "data_import_log")
})

test_that("database log table must be a valid table name", {
  path <- setup_config(log_table = "test invalid name")

  expect_error(db_config(path),
    ":example:log_table' must consist of only lower case letters and underscores")
})

