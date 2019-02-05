context("test-utils-assert")

test_that("can check for a function within an environment", {
  env <- new.env(parent = .GlobalEnv)
  expect_error(assert_func_exists("test", env),
               "Can't find function test within environment.")

  env$test <- function(){ "test func" }
  assert_func_exists("test", env)
  expect_equal(env$test(), "test func")
})

test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})

test_that("assert_character", {
  expect_error(assert_character(1), "must be character")
  expect_error(assert_character(TRUE), "must be character")
})

test_that("assert_scalar_character", {
  expect_error(assert_scalar_character(NULL), "must be a scalar")
  expect_error(assert_scalar_character(numeric(0)), "must be a scalar")
  expect_error(assert_scalar_character(1:2), "must be a scalar")
  expect_error(assert_scalar_character(1), "must be character")
  expect_error(assert_scalar_character(TRUE), "must be character")
  expect_silent(assert_scalar_character("test"))
})

test_that("assert_is", {
  expect_error(assert_is("x", "foo"), "must be a foo")
  expect_silent(assert_is(structure("x", class = "foo"), "foo"))
})

test_that("assert_file_exists", {
  path <- tempfile()
  expect_error(assert_file_exists(path), "File does not exist")
  writeLines(character(0), path)
  expect_silent(assert_file_exists(path))
})
