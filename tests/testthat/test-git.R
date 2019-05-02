context("git")

test_that("git parent directory can be located", {
  path <- build_git_demo()
  root <- git_root_directory(path)
  expect_equal(root, path)

  root <- git_root_directory(file.path(path, "example", "R"))
  expect_equal(root, path)

  path <- tempdir()
  expect_error(git_root_directory(path), sprintf(
    "Can't run import as can't locate git directory from path %s. %s", path,
    "Import must be under version control to be run."))

})

test_that("git status can be retrieved and clean can be checked", {
  path <- build_git_demo()
  expect_equal(git_status(path),
               list(success = TRUE, code = 0,
                    output = character(0), clean = TRUE))
  expect_true(git_repo_is_clean(path))

  writeLines("hello", file.path(path, "hello"))
  expect_equal(git_status(path),
               list(success = TRUE, code = 0,
                    output = "?? hello", clean = FALSE))
  expect_false(git_repo_is_clean(path))
})

test_that("git run returns errors", {
  path <- build_git_demo()
  writeLines("hello", file.path(path, "hello"))
  expect_error(git_run(c("test"), path, TRUE),
               "Error code 1 running command:[.\n]+")
})

