context("run-automatic-load")

test_that("pre and post load can be run", {
  path <- prepare_test_import("example_pre_post_load", add_data = TRUE,
                              add_job_table = TRUE)

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl(file.path(path, "example_pre_post_load"), db_name = "test")
  import$extract()
  import$transform()
  import$load()

  con <- import$get_connection()

  ## Preload was run
  data <- DBI::dbGetQuery(con,
                          "SELECT * FROM people WHERE name = 'Ed'")
  expect_true(length(data), 1)

  ## Post load was run
  indexes <- DBI::dbGetQuery(con, "PRAGMA  index_list('people')")
  expect_equal("people_name" %in% indexes$name)
})
