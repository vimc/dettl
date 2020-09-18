context("main")

test_that("parse arguments - default", {
  res <- main_parse_args("path")
  expect_equal(res,
               list(root = NULL,
                    args = list(import = "path", db_name = NULL, comment = NULL,
                                dry_run = FALSE, allow_dirty_git = FALSE)))
})


test_that("parse arguments - set lots", {
  res <- main_parse_args(c("--root=root", "--db-name=production",
                           "--comment", "comment", "--dry-run",
                           "--allow-dirty-git", "path"))
  expect_equal(res,
               list(root = "root",
                    args = list(import = "path", db_name = "production",
                                comment = "comment",
                                dry_run = TRUE, allow_dirty_git = TRUE)))
})


test_that("end to end", {
  path <- prepare_test_import()

  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  args <- c("--root", path, "--db-name", "test", "example")
  out <- main(args)

  expected_data <- data_frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187))
  colnames(expected_data) <- c("name", "age", "height")
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(path, "test.sqlite"))
  expect_equal(DBI::dbGetQuery(con, "SELECT name, age, height from people"),
               expected_data)
})
