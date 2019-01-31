context("test-dettl")

test_that("dettl works as expected", {

  ## Setup test db
  db_name <- "test.sqlite"
  create_test_db(db_name)
  on.exit(unlink(db_name))

  ## when creating import object
  import <- dettl::new_import("example/")

  ## object has been created
  expect_false(is.null(import))
  expect_equal(class(import), c("dataImport", "R6"))

  ## and connection and DB have been setup
  con <- import$get_connection()
  expect_true(!is.null(con) && DBI::dbIsValid(con))
  expect_true("people" %in% DBI::dbListTables(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## and no data has been extracted or transformed
  extracted_data <- import$get_extracted_data()
  expect_null(extracted_data, "Expected data non null")
  transformed_data <- import$get_transformed_data()
  expect_null(transformed_data, "Transformed data is non-null")

  ## when data is extracted
  extracted_data <- import$extract()
  expected_data <- data.frame(c("Alice", "Bob", "Clive"),
                              c(25, 43, 76),
                              c(175, 187, 163),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("name", "age", "height")

  ## data has been read from files
  expect_equal(length(extracted_data), 1)
  expect_equal(extracted_data$people, expected_data)

  ## transformed data is still null
  transformed_data <- import$get_transformed_data()
  expect_null(transformed_data, "Transformed data is non-null")

  ## when running transform
  transformed_data <- import$transform()

  ## transform data is available
  expect_equal(length(transformed_data), 1)
  expect_equal(transformed_data$people, expected_data[c(1,2), ])

  ## and DB is still empty
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## when testing data message is printed
  expect_message(import$test(), "All tests passed successfuly, can safely run load.")

  ## and DB is still empty
  expect_equal(DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1], 0)

  ## when load is run
  import$load()

  ## then database contains correct data
  expect_equal(DBI::dbGetQuery(con, "SELECT * from people"), expected_data[c(1,2), ])
})

test_that("run import runs a full import process", {

  ## Setup test db
  db_name <- "test.sqlite"
  create_test_db(db_name)
  on.exit(unlink(db_name))

  import <- dettl::run_import("example/")
  con <- import$get_connection()
  expected_data <- data.frame(c("Alice", "Bob"),
                              c(25, 43),
                              c(175, 187),
                              stringsAsFactors = FALSE)
  colnames(expected_data) <- c("name", "age", "height")
  expect_equal(DBI::dbGetQuery(con, "SELECT * from people"), expected_data)
})
