TestDataImport <- R6::R6Class(
  "TestDataImport",
  inherit = DataImport,
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

expect_no_error <- function(object, class = NULL, ..., info = NULL,
                            label = NULL) {
  expect_error(object, regexp = NA, class = class, ..., info = info,
               label = label)
}
