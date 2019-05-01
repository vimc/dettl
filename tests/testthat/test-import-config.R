context("import-config")

test_that("read config loads config from directory", {

  cfg <- read_config("example")
  expect_s3_class(cfg, "dettl_import_config")

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
  expect_length(cfg$load, 4)
  expect_true("func" %in% names(cfg$load))
  expect_true("test" %in% names(cfg$load))
  expect_true("verification_queries" %in% names(cfg$load))
  expect_is(cfg$load$func, "function")
  expect_equal(cfg$load$test, "R/test_load.R")
  expect_is(cfg$load$verification_queries, "function")

  expect_true("name" %in% names(cfg))
  expect_equal(cfg$name, "example")

  expect_false(cfg$load$default)

  expect_true("path" %in% names(cfg))
  expect_equal(cfg$path, "example")
})

test_that("read config adds missing fields from defaults", {
  cfg <- read_config("simple_example")
  expect_s3_class(cfg, "dettl_import_config")

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
  expect_length(cfg$load, 4)
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

test_that("read config uses default load when rewrite_keys are configured", {
  cfg <- read_config("example_default_load")

  expect_true("rewrite_keys" %in% names(cfg))
  expect_is(cfg$rewrite_keys, "ForeignKeyConstraints")
  expect_true("load" %in% names(cfg))
  expect_length(cfg$load, 3)
  expect_false("func" %in% names(cfg$load))
  expect_true(cfg$load$default)
  expect_true("test" %in% names(cfg$load))
  expect_true("verification_queries" %in% names(cfg$load))
  expect_is(cfg$load$verification_queries, "function")
  expect_equal(cfg$load$test, "R/test_load.R")
})

test_that("default load can be specified in config", {
  rewrite_keys <-
    "rewrite_keys:
    people:
      primary: id
      foreign:
        jobs: person"
  dir <- setup_dettl_config("default: true", rewrite_keys)
  cfg <- read_config(dir)
  expect_true(cfg$load$default)

  dir <- setup_dettl_config("default: FALSE")
  expect_error(read_config(dir),
               "Load stage must specify a load function OR use the default load function. Got default FALSE and NULL func TRUE.")

  dir <- setup_dettl_config("
  default: TRUE
  func: load", rewrite_keys)
  expect_error(read_config(dir),
               "Load stage must specify a load function OR use the default load function. Got default TRUE and NULL func FALSE.")
})