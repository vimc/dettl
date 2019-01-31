
dataImport <- R6::R6Class(
  "dataImport",
  cloneable = FALSE,

  private = list(
    path = NULL,
    con = NULL,
    extract_ = NULL,
    transform_ = NULL,
    test_ = NULL,
    load_ = NULL,
    extracted_data = NULL,
    transformed_data = NULL

  ),

  public = list(
    initialize = function(path, extract, transform, test, load,
                          rollback = NULL) {
      private$path <- path
      ## TODO: Only set up connection when it is actually needed
      private$con <- db_connect("destination", path)
      private$extract_ <- extract
      private$transform_ <- transform
      private$test_ <- test
      private$load_ <- load
    },

    rollback = function() {
    },

    extract = function() {
      if (!is.null(private$con) && DBI::dbIsValid(private$con)) {
        private$extracted_data <- private$extract_(private$path, private$con)
      } else {
        stop("DB connection is not valid cannot extract data")
      }
      private$extracted_data
    },

    transform = function() {
      if (is.null(private$extracted_data)) {
        self$extract()
      }
      private$transformed_data <- private$transform_(private$extracted_data)
      ## ...check that data looks sensible...
      if (length(private$transformed_data) == 0) {
        stop("Data transform failed, returned empty list.")
      }
      private$transformed_data
    },


    test = function() {
      if (is.null(private$transformed_data)) {
        self$transform()
      }
      ## testthat::test_dir(private$test_dir)
      private$test_(private$transformed_data, private$con)
      message("All tests passed successfuly, can safely run load.")
      invisible(TRUE)
    },

    load = function() {
      private$load_(private$transformed_data, private$con)
    },

    get_connection = function() {
      private$con
    },

    get_extracted_data = function() {
      private$extracted_data
    },

    get_transformed_data = function() {
      private$transformed_data
    }
  )
)
