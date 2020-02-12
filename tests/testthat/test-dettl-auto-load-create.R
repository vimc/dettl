context("dettl-auto-load-create")

test_that("dettl-auto-load-create can create new tables from df", {
  transformed_data <- list(
    people = data_frame(people = c("Alice", "Bob"),
                        age = c(45, 65)),
    abode = data_frame(name = c("Alice", "Bob"),
                       type = c("Igloo", "Geodesic dome"))
  )

  con <- get_local_connection()

  expect_message(
    res <- dettl_auto_load_create(transformed_data, con),
    "Creating table 'people' (2 rows x 2 columns)",
    fixed = TRUE)
  expect_true(res)

  expect_equal(DBI::dbGetQuery(con, "SELECT * from people"),
               transformed_data$people)
  expect_equal(DBI::dbGetQuery(con, "SELECT * from abode"),
               transformed_data$abode)

  ## auto-load-create does not allow appending
  transformed_data <- list(
    abode = data_frame(name = "Clive",
                       type = "Shed")
  )
  expect_error(dettl_auto_load_create(transformed_data, con),
               "table `abode` already exists")
})
