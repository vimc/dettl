context("foreign-key-constraints")

test_that("foreign key constraints can be initialised and accessed", {
  constraints <- list(
    "table" = list(
      "foreign" = list(
        "id" = list(
          "constrained_table" = "example_field",
          "constrained_table2" = "example_field2"
        )
      ),
      "serial" = "nid"
    ),
    "table2" = list(
      "foreign" = list(
        "id" = list(
          "constrained_table3" = "example_field3"
        )
      ),
      "serial" = c("id", "nid")
    )
  )
  mock_get_fk_constraints <- mockery::mock(NULL)
  mock_parse_constraints <- mockery::mock(constraints)
  with_mock("dettl:::get_fk_constraints" = mock_get_fk_constraints,
            "dettl:::parse_constraints" = mock_parse_constraints, {
    keys <- ForeignKeyConstraints$new("con")
  })

  expect_true(keys$used_as_foreign_key("table"))
  expect_false(keys$used_as_foreign_key("table3"))

  expect_equal(keys$get_referenced_keys("table"), "id")
  expect_error(keys$get_referenced_keys("table3"),
    "Tried to get referenced keys for table 'table3', table is missing from constraints.")

  expect_equal(keys$get_foreign_key_usages("table", "id"), list(
    "constrained_table" = "example_field",
    "constrained_table2" = "example_field2"
  ))
  expect_error(keys$get_foreign_key_usages("table3", "id"),
    "Tried to get foreign key usages for referenced table 'table3' and column 'id', table and column are missing from constraints.")

  expect_true(keys$has_serial("table"))
  expect_true(keys$has_serial("table2"))
  expect_false(keys$has_serial("table3"))
  expect_false(keys$is_serial("table", "id"))
  expect_true(keys$is_serial("table", "nid"))
  expect_equal(keys$is_serial("table", c("id", "nid")), c(FALSE, TRUE))
  expect_true(keys$is_serial("table2", "id"))
  expect_true(keys$is_serial("table2", "nid"))
  expect_equal(keys$is_serial("table2", c("id", "nid")), c(TRUE, TRUE))

  ## We can test if data to be uploaded contains constrained data
  data <- list(
    constrained_table3 = data_frame(
      test = "one",
      example_field3 = 2
    ),
    table_2 = data_frame(
      test = "two",
      example_field3 = 3
    )
  )
  expect_false(keys$is_serial_constraint("table", "id", data))
  expect_true(keys$is_serial_constraint("table2", "id", data))

  data <- list(
    table_1 = data_frame(
      test = "one",
      example_field3 = 2
    ),
    table_2 = data_frame(
      test = "two",
      example_field3 = 3
    )
  )
  expect_false(keys$is_serial_constraint("table2", "id", data))

  data <- list(
    constrained_table3 = data_frame(
      test = "one",
      field = 2
    ),
    table_2 = data_frame(
      test = "two",
      example_field3 = 3
    )
  )
  expect_false(keys$is_serial_constraint("table2", "id", data))

  expect_error(keys$is_serial_constraint("missing", "na", data),
               paste0("Tried to get foreign key usages for referenced table ",
                      "'missing' and column 'na', table and column are missing",
                      " from constraints"))
})

test_that("empty key constraints returns appropriate messages", {
  mock_get_fk_constraints <- mockery::mock(list())
  mock_parse_constraints <- mockery::mock(list())
  with_mock("dettl:::get_fk_constraints" = mock_get_fk_constraints,
            "dettl:::parse_constraints" = mock_parse_constraints, {
    keys <- ForeignKeyConstraints$new("con")
  })
  expect_false(keys$used_as_foreign_key("test"))

  expect_error(keys$get_referenced_keys("test"),
    "Tried to get referenced keys for table 'test', table is missing from constraints.")

  expect_error(keys$get_foreign_key_usages("test", "test2"),
    "Tried to get foreign key usages for referenced table 'test' and column 'test2', table and column are missing from constraints.")

  expect_false(keys$is_serial("table", "id"))
})

