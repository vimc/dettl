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
