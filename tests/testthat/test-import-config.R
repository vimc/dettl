context("import-config")

test_that("read config loads config from directory", {

  cfg <- read_config("example")
  expect_s3_class(cfg, "dettl_import_config")

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
  expect_length(cfg$load, 4)
  expect_true("func" %in% names(cfg$load))
  expect_true("test" %in% names(cfg$load))
  expect_true("verification_queries" %in% names(cfg$load))
  expect_is(cfg$load$func, "function")
  expect_equal(cfg$load$test, "R/test_load.R")
  expect_is(cfg$load$verification_queries, "function")

  expect_true("name" %in% names(cfg))
  expect_equal(cfg$name, "example")

  expect_false(cfg$load$automatic)

  expect_true("path" %in% names(cfg))
  expect_equal(cfg$path, "example")

  expect_equal(cfg$dettl$mode, "append")
  expect_true(cfg$dettl$transaction)
})

test_that("read config adds missing fields from defaults", {
  cfg <- read_config("simple_example")
  expect_s3_class(cfg, "dettl_import_config")

  expect_length(cfg, 7)

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

  expect_equal(cfg$dettl$mode, "append")
  expect_true(cfg$dettl$transaction)
})

test_that("read config fails if required configuration is not available", {
  expect_error(read_config("broken_example"),
               "No files found matching file pattern script.R")
})

test_that("wildcards in sources are expanded", {
  sources <- "R/*.R"
  files <- expand_wildcards(sources, "example")
  expect_equal(files, normalizePath(c("example/R/extract.R", "example/R/load.R",
                                      "example/R/test_extract.R",
                                      "example/R/test_load.R",
                                      "example/R/test_transform.R",
                                      "example/R/transform.R",
                                      "example/R/verification_queries.R"),
                                    winslash = "/"))

  sources <- c("example/R/extract.R", "example/R/load.R",
               "example/R/transform.R")
  files <- expand_wildcards(sources, ".")
  expect_length(files, 3)

  sources <- "no_match.R"
  expect_error(expand_wildcards(sources, "."),
               "No files found matching file pattern no_match.R")
})

test_that("automatic load can be specified in config", {
  dir <- setup_dettl_config("automatic: true")
  cfg <- read_config(dir)
  expect_true(cfg$load$automatic)

  dir <- setup_dettl_config("automatic: FALSE")
  expect_error(read_config(dir),
               "Load stage must specify a load function OR use the automatic load function. Got automatic FALSE and NULL func TRUE.")

  dir <- setup_dettl_config("
  automatic: TRUE
  func: load")
  expect_error(read_config(dir),
               "Load stage must specify a load function OR use the automatic load function. Got automatic TRUE and NULL func FALSE.")
})

test_that("error thrown if pre or post load used without automatic load", {
  dir <- setup_dettl_config("func: load_func\n  pre: pre_load")
  expect_error(read_config(dir),
               "Pre or post load are configured but using a custom load step. Pre and post load can only be used with automatic load.")

  dir <- setup_dettl_config("func: load_func\n  post: post_load")
  expect_error(read_config(dir),
               "Pre or post load are configured but using a custom load step. Pre and post load can only be used with automatic load.")
})

test_that("read config loads pre and post load functions in config", {

  cfg <- read_config("example_pre_post_load")
  expect_s3_class(cfg, "dettl_import_config")

  expect_length(cfg, 7)

  expect_true("load" %in% names(cfg))
  expect_length(cfg$load, 5)
  expect_true(cfg$load$automatic)
  expect_true("pre" %in% names(cfg$load))
  expect_true("post" %in% names(cfg$load))
  expect_is(cfg$load$pre, "function")
  expect_is(cfg$load$post, "function")
})

test_that("missing required function fields throw error", {
  fields <- list(extract = list(
    list(
      func = "func",
      must_exist = TRUE
    )
  ))
  config <- list()
  env <- new.env()
  expect_error(read_function_fields(fields, config, env),
               "Can't find required function func for field extract")
})

test_that("dettl config interprets import mode is invalid", {
  dir <- setup_dettl_config()
  cfg <- read_config(dir)
  expect_equal(cfg$dettl$mode, "append")

  dir <- setup_dettl_config(dettl = "dettl:
                                       mode: append")
  cfg <- read_config(dir)
  expect_equal(cfg$dettl$mode, "append")

  dir <- setup_dettl_config(dettl = "dettl:
                                       mode: create")
  cfg <- read_config(dir)
  expect_equal(cfg$dettl$mode, "create")

  dir <- setup_dettl_config(dettl = "dettl:
                                       mode: invalid")
  expect_error(read_config(dir),
               'Invalid mode - mode must be one of append, create got "invalid".')
})

test_that("dettl config transaction correctly", {
  dir <- setup_dettl_config()
  cfg <- read_config(dir)
  expect_true(cfg$dettl$transaction)

  dir <- setup_dettl_config(dettl = "dettl:
                                       transaction: TRUE")
  cfg <- read_config(dir)
  expect_true(cfg$dettl$transaction)

  dir <- setup_dettl_config(dettl = "dettl:
                                       transaction: FALSE")
  cfg <- read_config(dir)
  expect_false(cfg$dettl$transaction)

  dir <- setup_dettl_config(dettl = "dettl:
                                       transaction: invalid")
  cfg <- read_config(dir)
  expect_true(cfg$dettl$transaction)

  dir <- setup_dettl_config(dettl = "dettl:
                                       transaction: no")
  cfg <- read_config(dir)
  expect_false(cfg$dettl$transaction)
})
