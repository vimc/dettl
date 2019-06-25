context("default-load")

test_that("child tables can be updated", {

  ## Create test data
  col1 <- c(2, 3, 5)
  col2 <- c("one", "two", "three")
  parent_table <- data_frame(col1, col2)

  col1_child <- c(3, 2, 5)
  col3 <- c(6, 7, 8)
  child_table <- data_frame(col3, col1_child)

  col4 <- c(4,3,2)
  col5 <- c(TRUE, FALSE, FALSE)
  other_table <- data_frame(col4, col5)

  tables <- list(
    parent_table = parent_table,
    child_table = child_table,
    other_table = other_table
  )

  table_key_pair = list("child_table" = "col1_child")

  old_key_values = c(2, 3, 5)
  new_key_values = c(20, 30, 50)

  updated_tables <- update_child_tables(tables, table_key_pair, old_key_values,
                                        new_key_values)


  expect_equal(updated_tables$parent_table, parent_table)
  expect_equal(updated_tables$other_table, other_table)
  expect_equal(updated_tables$child_table$col3, updated_tables$child_table$col3)
  expect_equal(updated_tables$child_table$col1_child, c(30, 20, 50))
})

test_that("default load returns a function", {
  func <- get_default_load()
  expect_type(func, "closure")
})

test_that("default load supports 2 referenced fields within same table", {
  ## Note that we test this with 2 autoincrement fields in Postgres but
  ## this kind of configuration is not possible within SQLite as only an
  ## int primary key can autoincrement in sqlite.
  ## See https://www.sqlite.org/autoinc.html
  path <- prepare_test_import(create_db = FALSE)
  con <- prepare_example_postgres_db(add_multi_ref_fks = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ## There are multiple referenced keys
  rewrite_keys <- ForeignKeyConstraints$new(con)
  referenced_keys <- rewrite_keys$get_referenced_keys("referenced_table")

  ## Create test data
  referenced_table <- data_frame(id = c(1,2), nid = c(1,2))
  id_constraint <- data_frame(name = c("idRef1", "idRef2"), ref = c(1,2))
  nid_constraint <- data_frame(name = c("nidRef1", "nidRef2"), ref = c(2,1))

  tables <- list(
    referenced_table = referenced_table,
    id_constraint = id_constraint,
    nid_constraint = nid_constraint
  )

  ## Do load and check uploaded data
  default_load <- get_default_load()
  default_load(tables, con)

  ref_table <- DBI::dbGetQuery(con, "SELECT * FROM referenced_table")
  id_table <- DBI::dbGetQuery(con, "SELECT * FROM id_constraint")
  nid_table <- DBI::dbGetQuery(con, "SELECT * FROM nid_constraint")
  expect_equal(ref_table, referenced_table)
  expect_equal(id_table, id_constraint)
  expect_equal(nid_table, nid_constraint)
})

