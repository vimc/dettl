context("foreign-key-constraints")

test_that("foreign key constraints can be initialised and accessed", {
  constraints <- list(
    "table" = list(
      "foreign" = list(
        "id" = list(
          "constrained_table" = "example_field",
          "constrained_table2" = "example_field2"
        )
      )
    ),
    "table2" = list(
      "foreign" = list(
        "id" = list(
          "constrained_table3" = "example_field3"
        )
      )
    )
  )
  mock_get_fk_constraints <- mockery::mock(constraints)
  with_mock("dettl:::get_fk_constraints" = mock_get_fk_constraints, {
    keys <- ForeignKeyConstraints$new("con")
  })

  expect_true(keys$used_as_foreign_key("table"))
  expect_false(keys$used_as_foreign_key("table3"))

  expect_equal(keys$get_constrained_keys("table"), "id")
  expect_error(keys$get_constrained_keys("table3"),
    "Tried to get constrained keys for table 'table3', table is missing from constraints.")

  expect_equal(keys$get_foreign_key_usages("table", "id"), list(
    "constrained_table" = "example_field",
    "constrained_table2" = "example_field2"
  ))
  expect_error(keys$get_foreign_key_usages("table3", "id"),
    "Tried to get foreign key usages for constrained table 'table3' and column 'id', table and column are missing from constraints.")

})

test_that("empty key constraints returns appropriate messages", {
  mock_get_fk_constraints <- mockery::mock(list())
  with_mock("dettl:::get_fk_constraints" = mock_get_fk_constraints, {
    keys <- ForeignKeyConstraints$new("con")
  })
  expect_false(keys$used_as_foreign_key("test"))

  expect_error(keys$get_constrained_keys("test"),
    "Tried to get constrained keys for table 'test', table is missing from constraints.")

  expect_error(keys$get_foreign_key_usages("test", "test2"),
    "Tried to get foreign key usages for constrained table 'test' and column 'test2', table and column are missing from constraints.")
})
