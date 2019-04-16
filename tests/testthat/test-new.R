context("test-new")

testthat::test_that("create new creates directory and code template", {

  expect_false(file_exists("temp-test"))

  project_dir <- new_dettl("temp-test")
  ## Clear up temporary file
  on.exit(unlink(project_dir, recursive = TRUE))

  expect_equal(project_dir, paste(Sys.Date(), "temp_test", sep = "_"))
  expect_true(file_exists(project_dir))
  expect_true(file_exists(file.path(project_dir, "dettl.yml")))
  expect_true(file_exists(file.path(project_dir, "R")))
  expect_true(file_exists(file.path(project_dir, "R", "extract.R")))
  expect_true(file_exists(file.path(project_dir, "R", "transform.R")))
  expect_true(file_exists(file.path(project_dir, "R", "load.R")))
  expect_true(file_exists(file.path(project_dir, "tests", "test_extract.R")))
  expect_true(file_exists(file.path(project_dir, "tests", "test_transform.R")))
  expect_true(file_exists(file.path(project_dir, "tests", "test_load.R")))

  expect_error(new_dettl("temp-test"), sprintf(
    "Can't create new dettl process, failed to create directory with name %s from name temp-test. One may already exist.",
    project_dir))
})
