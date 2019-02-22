testDataImport <- R6::R6Class(
  "testDataImport",
  inherit = dataImport,
  cloneable = FALSE,

  public = list(
    initialize = function(path = NULL, extract = NULL, transform = NULL,
                          test_queries, test, load, transformed_data = NULL) {
      super$path <- path
      super$extract_ <- extract
      super$transform_ <- transform
      super$test_ <- test
      super$load_ <- load
      super$transformed_data <- transformed_data
    },

    ## Allow setting fields via public method for testing
    set_transformed_data = function(transformed_data) {
      super$transformed_data <- transformed_data
    },

    set_test = function(test) {
      super$test_ <- test
    },

    set_load = function(load) {
      super$load_ <- load
    },

    set_path = function(path) {
      super$path <- path
    }
  )
)

failing_test_import <- testDataImport$new(
  path = "example",
  transformed_data = data.frame(c("Alice", "Bob"),
                                c(25, 43),
                                c(175, 187),
                                stringsAsFactors = FALSE),
  load = function(transformed_data, con) {
    DBI::dbWriteTable(con, "people", transformed_data$people, append = TRUE)
  },
  test = "failing_load_test.R"
)

failing_test_import$set_path("exmaple_tests")

trigger_dbi_warning <- function() {
  oo <- options(warn = 0)
  on.exit(options(oo))
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  rm(con)
  suppressWarnings(gc())
}

trigger_dbi_warning()
