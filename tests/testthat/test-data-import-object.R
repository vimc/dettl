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
  call <- capture_args(dettl_run, "dettl_run", width = 50)
  expect_equal(call,
"    dettl_run(import, db_name = NULL,
        comment = NULL, dry_run = FALSE,
        allow_dirty_git = FALSE,
        stage = c(\"extract\", \"transform\"),
        ...)")
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

test_that("object cannot be created for unknown import language", {
  mockery::stub(dettl, "get_language", "test")
  expect_error(dettl("path", "db"),
               paste0("Can't initialise import for unknown language got ",
               "\"test\", language must be one of \"R\" or \"sql\"."))
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
