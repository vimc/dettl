context("data-write-error")

test_that("data write error exposes human readable message and data", {
  data <- data_frame(x = c("one", "two", "three"), y = seq_len(3))
  err <- data_write_error("Test message", "test_table", data)

  expect_equal(err$data, data)
  expect_match(err$message, paste0(
    "Failed trying to write data:\n",
    ".+",
    "\nto table 'test_table':\n",
    "Test message"))

  ## With large data frames
  data <- as.data.frame(matrix(runif(10000), nrow = 1000, ncol = 10))
  err <- data_write_error("Failed to write data", "test_table", data)

  expect_equal(err$data, data)
  expect_true(nchar(err$message) < 700)
})
