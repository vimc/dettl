context("generic-import")

test_that("generic import defines interface", {
  path <- prepare_test_import("example_pre_post_load", add_data = TRUE,
                              add_job_table = TRUE)
  import <- Import$new(path, NULL)
  res <- evaluate_promise(
    import$run_import(stage = c("extract", "transform", "load")))
  expect_true(any(grepl(
    "No extract function defined for this import, skipping step\n",
    res$messages, fixed = TRUE)))
  expect_true(any(grepl(
    "No transform function defined for this import, skipping step\n",
    res$messages, fixed = TRUE)))
  expect_true(any(grepl(
    "Running load",
    res$messages, fixed = TRUE)))
})
