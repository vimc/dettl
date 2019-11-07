context("git")

test_that("git parent directory can be located", {
  path <- build_git_demo()
  root <- git_root_directory(path)
  expect_equal(normalizePath(root), normalizePath(path))

  root <- git_root_directory(file.path(path, "example", "R"))
  expect_equal(normalizePath(root), normalizePath(path))

  path <- temp_dir()
  expect_error(git_root_directory(path), sprintf(
    "Can't run import as can't locate git directory from path %s. %s", path,
    "Import must be under version control to be run."), fixed = TRUE)

})

test_that("git user and email can be retrieved", {
  path <- build_git_demo()
  user <- git_user(path)
  expect_equal(user, "dettl")
  email <- git_email(path)
  expect_equal(email, "email@example.com")
})

test_that("git config with invalid field", {
  path <- build_git_demo()
  invalid_field <- "favourite_vegetable"
  expect_error(git_config(path, invalid_field),

    sprintf("'%s' not found in git config for path %s",
    invalid_field, path), fix = TRUE)
})

test_that("git branch can be retrieved", {
  path <- build_git_demo()
  branch <- git_branch(path)
  expect_equal(branch, "master")

  # Error thrown when HEAD is detatched
  writeLines("hello", file.path(path, "hello"))
  gert::git_add(files = ".", repo = path)
  author <- gert::git_signature_default(path)
  gert::git_commit(message = "second-import", repo = path, author = author)

  info <- gert::git_info(path)
  info$head <- "HEAD"

  fake_git_info <- mockery::mock(info)

  with_mock("gert::git_info" = fake_git_info, {
    expect_error(
      git_branch(path), sprintf(
    "Can't get current branch from path %s. %s", path,
    "Check repo is up to date and HEAD is not detached."), fix = TRUE)})
})

test_that("git hash can be retrieved", {
  path <- build_git_demo()
  hash <- git_hash(path)
  ## Hash of temp dir will change - check that we do indeed get a hash.
  expect_equal(nchar(hash), 40)
  expect_true(grepl("[0-9a-f]+", hash))
})
