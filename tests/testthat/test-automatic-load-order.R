context("automatic-load-order")

test_that("ordering can be retrieved", {
  graph <- list(b = NA,
                e = "k",
                k = c("b", "i", "j"),
                g = c("k", "h"),
                i = NA,
                j = NA,
                h = "i",
                c = c("j", "h"))
  order <- topological_order(graph)
  expect_equal(order,
               c("b", "i", "j", "k", "h", "e", "g", "c"))
})

test_that("cycled can be detected", {
  expect_error(topological_order(list(a = "a")),
               "A cyclic dependency detected for a:
  a: depends on a
Please write a custom load", fixed = TRUE)

  expect_error(topological_order(list(a = "b", b = "c", c = "a")),
               "A cyclic dependency detected for a, b, c:
  a: depends on b
  b: depends on c
  c: depends on a
Please write a custom load", fixed = TRUE)

  expect_error(topological_order(list(a = c("b", "c"), b = "c", c = "a")),
               "A cyclic dependency detected for a, b, c:
  a: depends on b, c
  b: depends on c
  c: depends on a
Please write a custom load", fixed = TRUE)
})

test_that("can build network table from fks", {
  constraint_table <- data_frame(
    constraint_table =  c("A",  "A",  "A",  "B",  "C",  "D",  "A",  "A",  "A"),
    constraint_column = c("A1", "A2", "A1", "B1", "C1", "D1", "A3", "A4", "A5"),
    referenced_table =  c("B",  "B",  "D",  "A",  "A",  "A",  "C",  "C",  "C")
  )
  data <- c("A", "B", "C", "E", "F")
  non_empty_columns <- list(
    A = c("A1", "A2", "A3"),
    B = "B1",
    C = "C1",
    E = "E1"
  )
  network <- get_network_dependencies(constraint_table, data,
                                      non_empty_columns)
  expect_equal(network, list(
    A = c("B", "C"),
    B = "A",
    C = "A",
    E = NA,
    F = NA
  ))
})

test_that("can get not-null columns from list of data frames", {
  data <- list(
    table1 = data_frame(
      A = c("one", "two"),
      B = c("three", NA),
      C = c(NA, NA)
    ),
    table2 = data_frame(
      A = c(NA, NA),
      B = c("four", "five")
    )
  )
  expect_equal(get_non_empty_columns(data), list(
    table1 = c("A", "B"),
    table2 = "B"
  ))
})
