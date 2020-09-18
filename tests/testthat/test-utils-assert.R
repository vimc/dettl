context("test-utils-assert")

test_that("can check for a function within an environment", {
  env <- new.env(parent = .GlobalEnv)
  expect_error(assert_func_exists("test", env),
               "Can't find function test within environment.")

  env$test <- function() {
    "test func"
  }
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
  path <- temp_file()
  expect_error(assert_file_exists(path), "File does not exist")
  writeLines(character(0), path)
  expect_silent(assert_file_exists(path))
})

test_that("assert_file_exists: error in case", {
  mockery::stub(assert_file_exists, "file_exists",
                structure(c(TRUE, FALSE, FALSE),
                          incorrect_case = c(FALSE, TRUE, FALSE),
                          correct_case = c("FOO" = "foo")))
  expect_error(assert_file_exists(c("bar", "FOO", "gaz")),
               "File does not exist: 'FOO' (should be 'foo'), 'gaz'",
               fixed = TRUE)
})

test_that("assert_named", {
  expect_error(assert_named(1), "must be named")
  expect_error(assert_named(stats::setNames(1:2, c("a", "a")), TRUE),
               "must have unique names")
  expect_silent(assert_named(stats::setNames(1:2, c("a", "a")), FALSE))
})
