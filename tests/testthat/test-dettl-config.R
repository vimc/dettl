context("dettl-config")

test_that("dettl config can be read and database connection info extracted", {
  cfg <- dettl_config("default_config")
  expect_s3_class(cfg, "dettl_config")

  ## default destination database:
  expect_equal(cfg$destination$driver, c("RSQLite", "SQLite"))
  expect_equal(cfg$destination$args, list(dbname = "dettl.sqlite"))

  expect_equal(cfg$source$driver, c("RSQLite", "SQLite"))
  expect_equal(cfg$source$args, list(dbname = "test.sqlite"))

  dest_dat <- dettl_db_args("destination", cfg)
  expect_identical(dest_dat$driver, RSQLite::SQLite)
  expect_identical(dest_dat$args$dbname, file.path(cfg$path, "dettl.sqlite"))

  source_dat <- dettl_db_args("source", cfg)
  expect_identical(source_dat$driver, RSQLite::SQLite)
  expect_identical(source_dat$args$dbname, file.path(cfg$path, "test.sqlite"))
})
