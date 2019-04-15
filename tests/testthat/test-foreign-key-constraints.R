context("foreign-key-constraints")

test_that("foreign key constraints can be initialised and accessed", {
  constraints <- list(
    "table" = list(
      "primary" = "id",
      "foreign" = list(
        "constrained_table" = "example_field",
        "constrained_table2" = "example_field2"
      )
    ),
    "table2" = list(
      "primary" = "id",
      "foreign" = list(
        "constrained_table3" = "example_field3"
      )
    )
  )
  keys <- ForeignKeyConstraints$new(constraints)

  expect_true(keys$used_as_foreign_key("table"))
  expect_false(keys$used_as_foreign_key("table3"))

  expect_equal(keys$get_primary_key("table"), "id")
  expect_error(keys$get_primary_key("table3"),
    "Tried to get primary key for table table3 not in configuration.")

  expect_equal(keys$get_foreign_key_usages("table"), list(
    "constrained_table" = "example_field",
    "constrained_table2" = "example_field2"
  ))
  expect_error(keys$get_foreign_key_usages("table3"),
    "Tried to get foreign key usages for primary key of table table3. Missing from configuration.")

})

test_that("misconfigured key constraints returns useful message", {
  constraints <- NULL
  expect_error(ForeignKeyConstraints$new(constraints),
    "Rewrite keys must be a named list with length > 0. Check configuration.")

  constraints <- list()
  expect_error(ForeignKeyConstraints$new(constraints),
    "Rewrite keys must be a named list with length > 0. Check configuration.")

  constraints <- list(
    "table" = list()
  )
  expect_error(ForeignKeyConstraints$new(constraints),
    "Rewrite keys must specify a primary key and foreign keys for each table. Check configuration for table table.")

  constraints <- list(
    "table" = list(
      "primary" = "id",
      "foreign" = list(
      )
    )
  )
  expect_error(ForeignKeyConstraints$new(constraints),
    "Foreign keys must be a named list with length > 0 for each table. Check configuration for table table.")

  constraints <- list(
    "table" = list(
      "primary" = "id",
      "foreign" = list(
        "constrained_table" = c(1,2)
      )
    )
  )
  expect_error(ForeignKeyConstraints$new(constraints),
    "Foreign keys must be a character. Check configuration for referenced table table and child table constrained_table.")
})
