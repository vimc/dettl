context("run-default-load")

test_that("default load can be run", {
  ## Set up a db with some people alread loaded
  db_name <- "test.sqlite"
  prepare_example_db(db_name, add_data = TRUE, add_job_table = TRUE)
  on.exit(unlink(db_name))

  ## Turn off reporting when running import so import tests do not print
  ## to avoid cluttering up test output.
  default_reporter <- testthat::default_reporter()
  options(testthat.default_reporter = "silent")
  on.exit(options(testthat.default_reporter = default_reporter), add = TRUE)

  import <- dettl("example_default_load/", default_load = TRUE,
                  db_name = "test")

  ## Setup mock INSERT query for testing. This is needed because in real
  ## impl we want to do an INSERT INTO RETURNING pattern but it is not possible
  ## to INSERT and return id in SQLite. But SQLite does make a
  ## "last_insert_rowid" function available so use this instead inside a mock
  ## impl to update the table and get the ID of the inserted row back.
  sql <- "INSERT INTO people
          (name, age, height)
          VALUES
          ($1, $2, $3)"
  con <- import$get_connection()
  insert_func <- function(x) {
    query <- DBI::dbSendQuery(con, sql, unname(x))
    DBI::dbClearResult(query)
    DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
  }
  run_import(import, c("extract", "transform"))
  data <- import$get_transformed_data()$people
  mock_insert <- mockery::mock(insert_func(data[1, c("name", "age", "height")]),
                               insert_func(data[2, c("name", "age", "height")]))

  ## Run load with mock INSERT function
  with_mock("dettl:::insert_data" = mock_insert, {
    run_import(import, "load")
  })

  data <- DBI::dbGetQuery(con,
    "SELECT p.id, p.name, j.job
     FROM people p
     LEFT JOIN jobs j on p.id = j.person")
  expected_data <- data.frame(c(1,2,3),
                              c("Daisy", "Alice", "Bob"),
                              c(NA, "developer", "researcher"),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("id", "name", "job")
  expect_equal(data, expected_data)
})
