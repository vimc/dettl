context("not-null-constraints")

test_that("not null constraints can be retrieved from sqlite database", {
  path <- prepare_test_import(add_job_table = TRUE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  not_null <- get_not_nullable(con)

  expected_not_null <- data_frame(
    table_name = c("jobs", "people", "people", "people"),
    column_name = c("id", "id", "name", "age")
  )
  expect_equal(not_null, expected_not_null)

  constraints <- NotNullConstraints$new(con)
  expect_true(constraints$is_nullable("dettl_import_log", "name"))
  expect_true(constraints$is_nullable("people", "height"))
  expect_false(constraints$is_nullable("people", "id"))
  expect_false(constraints$is_nullable("people", "name"))
  expect_false(constraints$is_nullable("people", "age"))
  expect_true(constraints$is_nullable("jobs", "job"))
  expect_true(constraints$is_nullable("missing_table", "missing_col"))
})


test_that("not null constraints can be retrieved from postgres database", {
  path <- prepare_test_import(create_db = FALSE)
  con <- prepare_example_postgres_db(add_job_table = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  not_null <- get_not_nullable(con)

  ## Postgres includes implicit not null constraint on primary keys
  expected_not_null <- data_frame(
    table_name = c("dettl_import_log", "jobs", "people", "people",
                   "people"),
    column_name = c("name", "id", "id", "name", "age")
  )
  expect_equal(not_null, expected_not_null)

  constraints <- NotNullConstraints$new(con)
  expect_false(constraints$is_nullable("dettl_import_log", "name"))
  expect_true(constraints$is_nullable("people", "height"))
  expect_false(constraints$is_nullable("people", "id"))
  expect_false(constraints$is_nullable("people", "name"))
  expect_false(constraints$is_nullable("people", "age"))
  expect_true(constraints$is_nullable("jobs", "job"))
  expect_true(constraints$is_nullable("missing_table", "missing_col"))
})
