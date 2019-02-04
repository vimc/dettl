
dataImport <- R6::R6Class(
  "dataImport",
  cloneable = FALSE,

  private = list(
    path = NULL,
    con = NULL,
    extract_ = NULL,
    transform_ = NULL,
    load_ = NULL,
    extract_test_ = NULL,
    transform_test_ = NULL,
    load_test_ = NULL,
    test_queries = NULL,
    extracted_data = NULL,
    transformed_data = NULL

  ),

  public = list(
    initialize = function(path, extract, extract_test, transform,
                          transform_test, load, load_test, test_queries,
                          rollback = NULL) {
      private$path <- path
      ## TODO: Only set up connection when it is actually needed
      private$con <- db_connect("destination", path)
      private$extract_ <- extract
      private$extract_test_ <- extract_test
      private$transform_ <- transform
      private$transform_test_ <- transform_test
      private$load_ <- load
      private$load_test_ <- load_test
      private$test_queries <- test_queries
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
        stop("Cannot run transform as no data has been extracted.")
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
        stop("Cannot run tests as no data has been transformed.")
      }
      ## testthat::test_dir(private$test_dir)
      private$test_(private$transformed_data, private$con)
      message("All tests passed successfuly, can safely run load.")
      invisible(TRUE)
    },

    load = function() {
      if (is.null(private$transformed_data)) {
        stop("Cannot run tests as no data has been transformed.")
      }
      run_load(private$load_, private$con, private$transformed_data,
               private$test_queries, private$path, private$load_test_)
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
