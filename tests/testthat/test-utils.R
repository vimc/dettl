context("utils")

test_that("yaml_read throws nicely", {
  expect_error(suppressWarnings(yaml_read("foo")), "while reading 'foo'")
})

test_that("string symbol parse", {
  expect_equal(check_symbol_from_str("a::b"), c("a", "b"))
  expect_error(check_symbol_from_str("a", "name"),
               "Expected fully qualified name for name")
  expect_error(check_symbol_from_str("a::b::c", "name"),
               "Expected fully qualified name for name")
})

test_that("Descend failure", {
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  expect_null(find_file_descend(".dettl_foobar", tempdir(), path))
  expect_null(find_file_descend(".dettl_foobar", "/", path))
  expect_null(find_file_descend(".dettl_foobar", "/", "/"))
})

test_that("copy failure", {
  path1 <- tempfile()
  path2 <- tempfile()
  writeLines("a", path1)
  writeLines("b", path2)
  on.exit(file.remove(path1, path2))
  expect_error(file_copy(path1, path2, overwrite = FALSE),
               "Error copying files")
  expect_equal(readLines(path2), "b")
})

test_that("platform detection", {
  expect_equal(is_windows(), Sys.info()[["sysname"]] == "Windows")
  expect_equal(is_linux(), Sys.info()[["sysname"]] == "Linux")
})

test_that("canonical case: single file", {
  root <- tempfile()
  dir.create(root)
  path <- "a"
  PATH <- toupper(path)
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(PATH))
    expect_equal(file_canonical_case(PATH), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  v <- file_exists(PATH, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: relative path", {
  root <- tempfile()
  dir.create(root)
  path <- file.path("a", "b", "c")
  PATH <- toupper(path)
  full <- file.path(root, path)

  dir.create(dirname(full), FALSE, TRUE)
  file.create(full)

  withr::with_dir(root, {
    expect_true(file_has_canonical_case(path))
    expect_equal(file_canonical_case(path), path)
    expect_true(file_exists(path))
    expect_true(file_exists(path, check_case = TRUE))

    expect_false(file_has_canonical_case(PATH))
    expect_equal(file_canonical_case(PATH), path)
  })

  expect_true(file_exists(path, check_case = FALSE, workdir = root))
  expect_true(file_exists(path, check_case = TRUE, workdir = root))

  expect_false(file_exists(PATH, check_case = TRUE, workdir = root))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }

  expect_true(file_exists(PATH, check_case = FALSE, workdir = root))
  v <- file_exists(PATH, check_case = TRUE, workdir = root,
                   force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: absolute path", {
  path <- file.path(tempfile(), "a", "b", "c")
  dir.create(dirname(path), FALSE, TRUE)
  file.create(path)
  path <- normalizePath(path, "/")
  PATH <- toupper(path)
  if (is_windows()) {
    ## On windows, use upper case drive letters here:
    path <- paste0(toupper(substr(path, 1, 1)),
                   substr(path, 2, nchar(path)))
  }

  expect_true(file_has_canonical_case(path))
  expect_equal(file_canonical_case(path), path)
  expect_true(file_exists(path))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_has_canonical_case(PATH))
  expect_equal(file_canonical_case(PATH), path)

  expect_true(file_exists(path, check_case = FALSE))
  expect_true(file_exists(path, check_case = TRUE))

  expect_false(file_exists(PATH, check_case = TRUE))

  if (is_linux()) {
    mockery::stub(file_exists, "file.exists", TRUE)
  }
  expect_true(file_exists(PATH, check_case = FALSE))

  v <- file_exists(PATH, check_case = TRUE, force_case_check = TRUE)
  expect_identical(attr(v, "incorrect_case"), TRUE)
  expect_equal(attr(v, "correct_case"), set_names(path, PATH))
})


test_that("canonical case: path splitting", {
  expect_equal(file_split_base("a/b/c"),
               list(path = c("a", "b", "c"), base = ".", absolute = FALSE))
  expect_equal(file_split_base("/a/b/c"),
               list(path = c("a", "b", "c"), base = "/", absolute = TRUE))
  expect_equal(file_split_base("c:/a/b/c"),
               list(path = c("a", "b", "c"), base = "c:/", absolute = TRUE))
  expect_equal(file_split_base("C:/a/b/c"),
               list(path = c("a", "b", "c"), base = "C:/", absolute = TRUE))
  expect_equal(file_split_base("C:/A/B/C", TRUE),
               list(path = c("a", "b", "c"), base = "C:/", absolute = TRUE))
})


test_that("canonical case: on missing file", {
  expect_equal(file_canonical_case("test-utils.R"), "test-utils.R")
  expect_identical(file_canonical_case("another file"), NA_character_)
})

test_that("check_fields returns meaningful errors", {
  fields <- list()
  fields$field1 <- "field1"
  required <- c("field2")
  optional <- c("field3")

  expect_error(check_fields(fields, "test", required, optional),
               "Fields missing from test: field2")

  fields$field2 <- "field2"

  expect_error(check_fields(fields, "test", required, optional),
               "Unknown fields in test: field1")
})

test_that("check_length returns meaningful errors", {
  test_list <- list(test1 = "123", test2 = "456")

  expect_error(check_length(test_list, "gt", 3),
               "Length of test_list must be greater than 3.")

  expect_error(check_length(test_list, "lt", 1),
               "Length of test_list must be less than 1.")
})
