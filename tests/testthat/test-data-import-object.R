context("import-object")

test_that("format", {
  path <- prepare_test_import()
  import <- dettl(file.path(path, "example/"), "test")
  private <- environment(import$initialize)$private
  expect_equal(import$format(FALSE)[[1]], "<dettl: RImport>")
  expect_equal(import$format(TRUE), "Data import object")
})

test_that("format works for functions with many args", {
  ## We're arbitraily choosing to use run_load func here as it has
  ## many arguments
  call <- capture_args(run_load, "run_load", width = 50)
  expect_equal(call,
"    run_load(con, load, extracted_data,
        transformed_data, test_queries,
        pre_load, post_load, path,
        test_file, log_table, comment)")
})


test_that("help: RImport", {
  path <- prepare_test_import()
  import <- dettl(file.path(path, "example/"), "test")
  mock_help <- mockery::mock(NULL)
  mockery::stub(import$help, "utils::help", mock_help)
  import$help()
  args <- mockery::mock_args(mock_help)[[1]]
  expect_equal(args, list("RImport", package = "dettl"))
})

test_that("object cannot be created for unknown import mode", {
  mockery::stub(dettl, "get_mode", "test")
  expect_error(dettl("path", "db"),
               paste0("Can't initialise import for unknown mode got \"test\", ",
               "mode must be one of \"create\" or \"append\"."))
})

test_that("format generic import", {
  path <- prepare_test_import()
  import <- Import$new(file.path(path, "example/"), "test")
  private <- environment(import$initialize)$private
  expect_equal(import$format(FALSE)[[1]], "<dettl: Import>")
  expect_equal(import$format(TRUE), "Data import object")
})

test_that("help: generic import", {
  path <- prepare_test_import()
  import <- Import$new(file.path(path, "example/"), "test")
  mock_help <- mockery::mock(NULL)
  mockery::stub(import$help, "utils::help", mock_help)
  import$help()
  args <- mockery::mock_args(mock_help)[[1]]
  expect_equal(args, list("Import", package = "dettl"))
})
