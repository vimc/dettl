context("default-load")

test_that("child tables can be updated", {

  ## Create test data
  data_frame <- function(...) { data.frame(..., stringsAsFactors = FALSE) }
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
  func <- get_default_load(keys)
  expect_type(func, "closure")
})
