
DataImport <- R6::R6Class(
  "DataImport",
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
                          db_name, rollback = NULL) {
      private$path <- path
      ## TODO: Only set up connection when it is actually needed
      private$con <- db_connect(db_name, path)
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
      private$extracted_data <- run_extract(private$con, private$extract_,
                                           private$path, private$extract_test_)
    },

    transform = function() {
      private$transformed_data <- run_transform(private$con, private$transform_,
                                           private$path, private$extracted_data,
                                           private$transform_test_)
    },

    load = function(dry_run) {
      run_load(private$con, private$load_, private$transformed_data,
               private$test_queries, private$path, private$load_test_, dry_run)
      invisible(TRUE)
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
