context("primary-foreign-keys")

test_that("postgres constraints can be retireved", {
  con <- prepare_example_postgres_db(add_fk_data = TRUE)
  on.exit(DBI::dbDisconnect(con))

  constraints <- get_fk_constraints(con)

  expect_equal(names(constraints), c("region", "street"))
  expect_equal(constraints$street$primary, "name")
  expect_equal(names(constraints$street$foreign), "address")
  expect_equal(constraints$street$foreign$address, "street")

  expect_equal(constraints$region$primary, "name")
  expect_true(
    all(names(constraints$region$foreign) %in% c("address", "region")))
  expect_equal(constraints$region$foreign$address, "region")
  expect_equal(constraints$region$foreign$region, "parent")
})

test_that("sqlite constraints can be retireved", {
  path <- prepare_test_import(add_fk_data = TRUE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  constraints <- get_fk_constraints(con)

  expect_equal(names(constraints), c("region", "street"))
  expect_equal(constraints$street$primary, "name")
  expect_equal(names(constraints$street$foreign), "address")
  expect_equal(constraints$street$foreign$address, "street")

  expect_equal(constraints$region$primary, "name")
  expect_true(
    all(names(constraints$region$foreign) %in% c("address", "region")))
  expect_equal(constraints$region$foreign$address, "region")
  expect_equal(constraints$region$foreign$region, "parent")
})

test_that("postgres foreign key constraints can be read", {
  con <- prepare_example_postgres_db(add_fk_data = TRUE)
  on.exit(DBI::dbDisconnect(con))

  constraints <- get_postgres_fk(con)

  expect_true("constraint_table" %in% colnames(constraints))
  expect_true("constraint_column" %in% colnames(constraints))
  expect_true("referenced_table" %in% colnames(constraints))
  expect_true("referenced_column" %in% colnames(constraints))
  expect_true("constraint_type" %in% colnames(constraints))
  expect_equal(nrow(constraints), 3)
  expect_true(all(constraints$constraint_table %in% c("address", "address", "region")))
  expect_true(all(constraints$constraint_column %in% c("region", "street", "parent")))
  expect_true(all(constraints$referenced_table %in% c("region", "street", "region")))
  expect_true(all(constraints$referenced_column == "name"))
})


test_that("sqlite foreign key constraints can be read", {
  path <- prepare_test_import(add_fk_data = TRUE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  constraints <- get_sqlite_fk(con)

  expect_true("constraint_table" %in% colnames(constraints))
  expect_true("constraint_column" %in% colnames(constraints))
  expect_true("referenced_table" %in% colnames(constraints))
  expect_true("referenced_column" %in% colnames(constraints))
  expect_equal(nrow(constraints), 3)
  expect_true(all(constraints$constraint_table %in% c("address", "address", "region")))
  expect_true(all(constraints$constraint_column %in% c("region", "street", "parent")))
  expect_true(all(constraints$referenced_table %in% c("region", "street", "region")))
  expect_true(all(constraints$referenced_column == "name"))
})

test_that("constraints can be parsed", {
  data <- data.frame(
    constraint_table = c("table1", "table2", "table2", "table3"),
    constraint_column = c("col1", "col2", "col3", "col4"),
    referenced_table = c("reftable1", "reftable1", "reftable2", "reftable3"),
    referenced_column = c("id", "id", "name", "id"),
    stringsAsFactors = FALSE
  )
  constraints <- parse_constraints(data)

  constraint_list <- list(
    "reftable1" = list(
      primary = "id",
      foreign = list(
        "table1" = "col1",
        "table2" = "col2"
      )
    ),
    "reftable2" = list(
      primary = "name",
      foreign = list(
        "table2" = "col3"
      )
    ),
    "reftable3" = list(
      primary = "id",
      foreign = list(
        "table3" = "col4"
      )
    )
  )
  expect_equal(constraints, constraint_list)
})

test_that("table can have more than 1 column constrained on same field", {
  data <- data.frame(
    constraint_table = c("table1", "table1"),
    constraint_column = c("col1", "col2"),
    referenced_table = c("reftable1", "reftable1"),
    referenced_column = c("id", "id"),
    stringsAsFactors = FALSE
  )
  constraints <- parse_constraints(data)

  expect_equal(names(constraints), "reftable1")
  expect_equal(constraints$reftable1$primary, "id")
  expect_equal(names(constraints$reftable1$foreign), "table1")
  expect_length(constraints$reftable1$foreign$table1, 2)
})

test_that("multiple keys can be referenced from each table", {
  data <- data.frame(
    constraint_table = c("table1", "table2"),
    constraint_column = c("col1", "col2"),
    referenced_table = c("reftable1", "reftable1"),
    referenced_column = c("id", "id2"),
    stringsAsFactors = FALSE
  )
  constraints <- parse_constraints(data)

  expect_equal(names(constraints), "reftable1")
  expect_equal(constraints$reftable1$primary, c("id", "id2"))
  expect_equal(names(constraints$reftable1$foreign), c("table1", "table2"))
  expect_equal(constraints$reftable1$foreign$table1, "col1")
  expect_equal(constraints$reftable1$foreign$table2, "col2")
})

test_that("unsupported sql dialect returns useful error", {
  mock_sql_dialect <- mockery::mock("Unsupported-dialect")

  with_mock("dettl:::sql_dialect" = mock_sql_dialect, {
    expect_error(get_fk_constraints("Unsupported-con"),
      "Only sqlite and postgresql dialects are supported got Unsupported-dialect.")
  })
})

test_that("empty list returned when no constraints", {
  path <- prepare_test_import(add_fk_data = FALSE)
  con <- dbi_db_connect(RSQLite::SQLite(), file.path(path, "test.sqlite"))

  constraints <- get_fk_constraints(con)

  expect_equal(constraints, list())
})
