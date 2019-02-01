context("test-utils-assert")

test_that("can check for a function within an environment", {
  env <- new.env(parent = .GlobalEnv)
  expect_error(assert_func_exists("test", env),
               "Can't find function test within environment.")

  env$test <- function(){ "test func" }
  assert_func_exists("test", env)
  expect_equal(env$test(), "test func")
})
