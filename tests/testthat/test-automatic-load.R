context("automatic-load")

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
                                        new_key_values, "parent_table")


  expect_equal(updated_tables$parent_table, parent_table)
  expect_equal(updated_tables$other_table, other_table)
  expect_equal(updated_tables$child_table$col3, updated_tables$child_table$col3)
  expect_equal(updated_tables$child_table$col1_child, c(30, 20, 50))
})

test_that("automatic load supports 2 referenced fields within same table", {
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
  expect_equal(referenced_keys, c("id", "nid"))

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
  dettl_auto_load(tables, con)

  ref_table <- DBI::dbGetQuery(con, "SELECT * FROM referenced_table")
  id_table <- DBI::dbGetQuery(con, "SELECT * FROM id_constraint")
  nid_table <- DBI::dbGetQuery(con, "SELECT * FROM nid_constraint")
  expect_equal(ref_table, referenced_table)
  expect_equal(id_table, id_constraint)
  expect_equal(nid_table, nid_constraint)
})

test_that("postgres automatic load works as expected", {
  path <- prepare_test_import(create_db = FALSE)
  con <- prepare_example_postgres_db(add_fk_data = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ## Create test data
  region <- data_frame(id = c(5,6), name = c("France", "Paris"))
  street <- data_frame(name = "Test Avenue")
  address <- data_frame(street = "Test Avenue", region = 5)

  tables <- list(
    region = region,
    street = street,
    address = address
  )

  ## Do load and check uploaded data
  expect_message(
    dettl_auto_load(tables, con),
    "Updating region (adding 2 rows)",
    fixed = TRUE)

  ## Create expected data
  db_region <- data_frame(id = c(1,2,3,4),
                          name = c("UK", "London", "France", "Paris"),
                          parent = c(NA, 1, NA, NA))
  db_street <- data_frame(name = c("Commercial Road", "The Street",
                                   "Test Avenue"))
  db_address <- data_frame(street = c("The Street", "Test Avenue"),
                           region = c(2, 3))

  region_table <- DBI::dbGetQuery(con, "SELECT * FROM region")
  street_table <- DBI::dbGetQuery(con, "SELECT * FROM street")
  address_table <- DBI::dbGetQuery(con, "SELECT * FROM address")
  expect_equal(region_table, db_region)
  expect_equal(street_table, db_street)
  expect_equal(address_table, db_address)

  ## Trying to upload with same serial PK again works
  tables <- list(region = region)
  dettl_auto_load(tables, con)

  db_region <- data_frame(
    id = c(1,2,3,4,5,6),
    name = c("UK", "London", "France", "Paris", "France", "Paris"),
    parent = c(NA, 1, NA, NA, NA, NA))
  region_table <- DBI::dbGetQuery(con, "SELECT * FROM region")
  expect_equal(region_table, db_region)

  ## Trying to upload with same non-serial PK again fails
  tables <- list(street = street)
  expect_error(dettl_auto_load(tables, con), class = "dettl_data_write_error")
})

test_that("sqlite automatic load works as expected", {
  path <- prepare_test_import(add_fk_data = TRUE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  ## Create test data
  region <- data_frame(id = c(5,6), name = c("France", "Paris"))
  street <- data_frame(name = "Test Avenue")
  address <- data_frame(street = "Test Avenue", region = 5)

  tables <- list(
    region = region,
    street = street,
    address = address
  )

  ## Do load and check uploaded data
  dettl_auto_load(tables, con)

  ## Create expected data
  db_region <- data_frame(id = c(1,2,3,4),
                          name = c("UK", "London", "France", "Paris"),
                          parent = c(NA, 1, NA, NA))
  db_street <- data_frame(name = c("Commercial Road", "The Street",
                                   "Test Avenue"))
  db_address <- data_frame(street = c("The Street", "Test Avenue"),
                           region = c(2, 3))

  region_table <- DBI::dbGetQuery(con, "SELECT * FROM region")
  street_table <- DBI::dbGetQuery(con, "SELECT * FROM street")
  address_table <- DBI::dbGetQuery(con, "SELECT * FROM address")
  expect_equal(region_table, db_region)
  expect_equal(street_table, db_street)
  expect_equal(address_table, db_address)

  ## Trying to upload with same serial PK again works
  tables <- list(region = region)
  dettl_auto_load(tables, con)

  db_region <- data_frame(
    id = c(1,2,3,4,5,6),
    name = c("UK", "London", "France", "Paris", "France", "Paris"),
    parent = c(NA, 1, NA, NA, NA, NA))
  region_table <- DBI::dbGetQuery(con, "SELECT * FROM region")
  expect_equal(region_table, db_region)

  ## Trying to upload with same non-serial PK again fails
  tables <- list(street = street)
  expect_error(dettl_auto_load(tables, con), class = "dettl_data_write_error")
})

test_that("sqlite automatic load works with cyclic fks", {
  path <- prepare_test_import(add_cyclic_fks = TRUE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  ## Create test data
  model <- data_frame(id = c("one", "two"), current_version = c(NA, NA))
  model_version <- data_frame(id = c(4, 5), model = c("one", "two"))

  tables <- list(
    model = model,
    model_version = model_version
  )

  dettl_auto_load(tables, con)

  ## Create expected data
  expected_model <- data_frame(id = c("one", "two"),
                               current_version = c(NA_integer_, NA_integer_))
  expected_model_version <- data_frame(id = c(1, 2),
                                       model = c("one", "two"))

  model_table <- DBI::dbGetQuery(con, "SELECT * FROM model")
  model_version_table <- DBI::dbGetQuery(con, "SELECT * FROM model_version")
  expect_equal(model_table, expected_model)
  expect_equal(model_version_table, expected_model_version)

  ## If instead using an empty column
  model <- data_frame(id = c("three", "four"))
  model_version <- data_frame(id = c(4, 5), model = c("three", "four"))
  tables <- list(
    model = model,
    model_version = model_version
  )
  dettl_auto_load(tables, con)

  ## Create expected data
  expected_model <- data_frame(id = c("one", "two", "three", "four"),
                               current_version = rep(NA_integer_, 4))
  expected_model_version <- data_frame(id = c(1, 2, 3, 4),
                                       model = c("one", "two", "three", "four"))

  model_table <- DBI::dbGetQuery(con, "SELECT * FROM model")
  model_version_table <- DBI::dbGetQuery(con, "SELECT * FROM model_version")
  expect_equal(model_table, expected_model)
  expect_equal(model_version_table, expected_model_version)

})

test_that("postgres automatic load works with cyclic fks", {
  path <- prepare_test_import(create_db = FALSE)
  con <- prepare_example_postgres_db(add_cyclic_fks = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ## Create test data
  model <- data_frame(id = c("one", "two"),
                      current_version = c(NA_integer_, NA_integer_))
  model_version <- data_frame(id = c(4, 5), model = c("one", "two"))

  tables <- list(
    model = model,
    model_version = model_version
  )

  dettl_auto_load(tables, con)

  ## Create expected data
  expected_model <- data_frame(id = c("one", "two"),
                               current_version = c(NA_integer_, NA_integer_))
  expected_model_version <- data_frame(id = c(1, 2),
                                       model = c("one", "two"))

  model_table <- DBI::dbGetQuery(con, "SELECT * FROM model")
  model_version_table <- DBI::dbGetQuery(con, "SELECT * FROM model_version")
  expect_equal(model_table, expected_model)
  expect_equal(model_version_table, expected_model_version)

  ## If instead using an empty column
  model <- data_frame(id = c("three", "four"))
  model_version <- data_frame(id = c(4, 5), model = c("three", "four"))
  tables <- list(
    model = model,
    model_version = model_version
  )
  dettl_auto_load(tables, con)

  ## Create expected data
  expected_model <- data_frame(id = c("one", "two", "three", "four"),
                               current_version = rep(NA_integer_, 4))
  expected_model_version <- data_frame(id = c(1, 2, 3, 4),
                                       model = c("one", "two", "three", "four"))

  model_table <- DBI::dbGetQuery(con, "SELECT * FROM model")
  model_version_table <- DBI::dbGetQuery(con, "SELECT * FROM model_version")
  expect_equal(model_table, expected_model)
  expect_equal(model_version_table, expected_model_version)

})

test_that("map values works as expected", {
  data <- c(1, 3, 2, 2)
  old <- c(1, 2, 3)
  new <- c(4, 5, 6)

  mapped_values <- map_values(data, old, new, "table_name", "col_name")
  expect_equal(mapped_values, c(4, 6, 5, 5))

  mapped_values <- map_values(data, old, new, "table_name", "col_name")
  expect_equal(mapped_values, c(4, 6, 5, 5))

  expect_error(map_values(data, c(1, 2), new, "table_name", "col_name"))

  data <- c(4, NA, 2)
  old <- c(2, 3, 4)
  new <- c(3, 4, 5)

  mapped_values <- map_values(data, old, new, "table_name", "col_name")
  expect_equal(mapped_values, c(5, NA, 3))

  data <- c(1, 2, 2, NA, 4, 4, 5)
  old <- c(4, 5)
  new <- c(6, 7)

  mapped_values <- map_values(data, old, new, "table_name", "col_name")
  expect_equal(mapped_values, c(1, 2, 2, NA, 6, 6, 7))
})

test_that("automatic load supports upload without specifying referenced keys", {
  path <- prepare_test_import(create_db = FALSE)
  con <- prepare_example_postgres_db(add_fk_data = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  ## When uploading data where serial PK is used as a referenced key in foreign
  ## key constraint but table with FK on is NOT being uploaded too
  region <- data_frame(name = c("France", "Paris"))

  tables <- list(
    region = region
  )

  ## Then data can be uploaded
  dettl_auto_load(tables, con)

  expected_region <- data_frame(id = c(1, 2, 3, 4),
                               name = c("UK", "London", "France", "Paris"),
                               parent = c(NA, 1, NA, NA))

  region_table <- DBI::dbGetQuery(con, "SELECT * FROM region")
  expect_equal(region_table, expected_region)
})

test_that("automatic load supports upload without specifying referenced keys", {
  path <- prepare_test_import(add_fk_data = TRUE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  ## When uploading data where serial PK is used as a referenced key in foreign
  ## key constraint but table with FK on is NOT being uploaded too
  region <- data_frame(name = c("France", "Paris"))

  tables <- list(
    region = region
  )

  ## Then data can be uploaded
  dettl_auto_load(tables, con)

  expected_region <- data_frame(id = c(1, 2, 3, 4),
                                name = c("UK", "London", "France", "Paris"),
                                parent = c(NA, 1, NA, NA))

  region_table <- DBI::dbGetQuery(con, "SELECT * FROM region")
  expect_equal(region_table, expected_region)

  ## When uploading data where serial PK is used as a referenced key in foreign
  ## key constraints and table with FK on IS being uploaded too
  region <- data_frame(name = c("Germany"))
  address <- data_frame(street = "Street", region = 5)

  tables <- list(
    region = region,
    address = address
  )

  ## then PK must be specified
  expect_error(dettl_auto_load(tables, con),
               paste0("Can't uploaded data, referenced key 'id' of table ",
                      "'region' is missing but is referenced by foreign key ",
                      "constraint used in data."))

})
