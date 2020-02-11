context("mode")

test_that("can check mode is valid", {
  expect_equal(check_valid_mode("create"), "create")
  expect_equal(check_valid_mode("append"), "append")
  expect_equal(check_valid_mode(NULL), "append")
  expect_error(check_valid_mode("invalid"),
               'Invalid mode - mode must be one of append, create got "invalid".')
})

test_that("can check whether tables can be created for modes", {
  expect_true(allow_create_table("create"))
  expect_false(allow_create_table("append"))
})

test_that("can get appropriate auto load function from mode", {
  expect_equal(get_auto_load_function("create"), dettl_auto_load_create)
  expect_equal(get_auto_load_function("append"), dettl_auto_load)
})
