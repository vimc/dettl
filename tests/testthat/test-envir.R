context("envir")

testthat::test_that("envir variables can be read from cfg", {
  path <- temp_file()
  dir.create(path)
  filename <- file.path(path, "dettl_envir.yml")
  writeLines("TEST: 123", filename)

  envir <- envir_read(path)
  expect_equal(envir, stats::setNames("123", "TEST"))

  writeLines("TEST: example_envir\nTEST2: example_envir2", filename)
  envir <- envir_read(path)
  expect_equal(envir, stats::setNames(c("example_envir", "example_envir2"),
                               c("TEST", "TEST2")))

  writeLines("TEST:\n  - example_envir\n  - example_envir2", filename)
  expect_error(envir_read(path),
    "Expected all elements of dettl_envir.yml to be scalar (check 'TEST')",
    fixed = TRUE)
})
