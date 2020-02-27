context("connection-cleanup")

test_that("connection is cleaned up if reload called when in a transaction", {
  ## In case there are any database connections waiting on garbage collection
  gc()

  path <- build_git_demo("example", "dettl_config.yml")
  con <- prepare_example_postgres_db()
  ## Close this connection as the import creates a new one below
  DBI::dbDisconnect(con)

  import <- dettl(file.path(path, "example"), db_name = "psql_test")
  con <- import$get_connection()

  ## Start a transaction
  import$begin_transaction()
  connections <- get_connections(con)
  connection_no_start <- nrow(connections)

  ## Reload the database connection
  expect_message(import$reload(), "Rolling back active transaction")

  ## No 'idle in transaction' connections exist and number of connections is
  ## constant
  new_connection <- import$get_connection()
  connections <- get_connections(new_connection)
  expect_equal(nrow(connections), connection_no_start)
  expect_false(any(connections$state == "idle in transaction"))

  ## Old connection has been closed
  expect_false(DBI::dbIsValid(con))
})
