context("initialise")

test_that("table is initialised and success message returned", {
  path <- prepare_example_db(temp_dir(), add_log_table = FALSE)
  mock_db_connect <- mockery::mock(DBI::dbConnect(RSQLite::SQLite(), path),
                                   cycle = TRUE)

  with_mock("dettl::db_connect" = mock_db_connect, {
    expect_message(dettl_db_create_log_table(".", "test"),
                   "Creating log table in DB test.")
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    expect_true(DBI::dbExistsTable(con, "dettl_import_log"))

    expect_error(dettl_db_create_log_table(".", "test"),
                 "table dettl_import_log already exists")
  })
})

test_that("table is initialised and success message returned postgres", {
  con <- prepare_example_postgres_db(FALSE)
  mock_db_connect <- mockery::mock(con, cycle = TRUE)
  with_mock("dettl::db_connect" = mock_db_connect, {
    expect_message(dettl_db_create_log_table(".", "test"),
                   "Creating log table in DB test.")
    con <- get_postgres_connection("dettl_test_db", "postgres", "localhost")
    expect_true(DBI::dbExistsTable(con, "dettl_import_log"))

    expect_error(dettl_db_create_log_table(".", "test"),
      'Failed to fetch row: ERROR:  relation "dettl_import_log" already exists\n')
  })
})

test_that("initialising non SQLite or Postgrs throws an error", {
  con <- list(x = "fake con")
  class(con) <- "FakeCon"
  ## Mock away setting up and disconnecting as we're interested in the error
  ## thrown when initialise doesn't support the type.
  mock_db_connect <- mockery::mock(con)
  mock_db_disconnect <- mockery::mock(invisible(TRUE))

  with_mock("dettl:::db_connect" = mock_db_connect,
            "DBI::dbDisconnect" = mock_db_disconnect, {
    expect_error(
      dettl_db_create_log_table(".", "test"),
      "Can't initialise DB test as not a SQLite or Postgres type. Got FakeCon.")
  })
})
