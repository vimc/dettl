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
    "Import must be under version control to be run."), fixed = TRUE)

})

test_that("git status can be retrieved and clean can be checked", {
  path <- build_git_demo()
#  expect_equal(git_status(path),
#               list(success = TRUE, code = 0,
#                    output = character(0), clean = TRUE))
  expect_true(git_repo_is_clean(path))

  writeLines("hello", file.path(path, "hello"))
#  expect_equal (git_status(path),
#               list(success = TRUE, code = 0,
#                    output = "?? hello", clean = FALSE))
  expect_false(git_repo_is_clean(path))
})

#test_that("git run returns errors", {
#  path <- build_git_demo()
#  writeLines("hello", file.path(path, "hello"))
#  expect_error(git_run(c("test"), path, TRUE),
#               "Error code 1 running command:[.\n]+")
#})

test_that("git user and email can be retrieved", {
  path <- build_git_demo()
  user <- git_user(path)
  expect_equal(user, "dettl")
  email <- git_email(path)
  expect_equal(email, "email@example.com")
})

test_that("git branch can be retrieved", {
  path <- build_git_demo()
  branch <- git_branch(path)
  expect_equal(branch, "master")

  ## Error thrown when HEAD is detatched
  writeLines("hello", file.path(path, "hello"))
  gert::git_add(files = ".", repo = path)
  author <- gert::git_signature_default(path)
  gert::git_commit(message = "second-import", repo = path, author = author)

  gert::git_branch_checkout(branch = "HEAD~1", repo = path)
  expect_error(git_branch(path), sprintf(
    "Can't get current branch from path %s. %s", path,
    "Check repo is up to date and HEAD is not detatched."))
})

test_that("git hash can be retrieved", {
  path <- build_git_demo()
  hash <- git_hash(path)
  ## Hash of temp dir will change - check that we do indeed get a hash.
  expect_equal(nchar(hash), 40)
  expect_true(grepl("[0-9a-f]+", hash))
})
