##' Manage data import.
##'
##' @template DataImport
##'
##' @title Data Import
##' @name DataImport
##'
##' @examples
##'
##' path <- dettl:::prepare_test_import(
##' system.file("examples", "person_information", package = "dettl"),
##' system.file("examples", "dettl_config.yml", package = "dettl"))
##' import_path <- file.path(path, "person_information")
##'
##' import <- dettl::dettl(import_path, db_name = "test")
##' import$extract()
##' import$transform()
##' import$load()
NULL

DataImport <- R6::R6Class(
  "DataImport",
  cloneable = FALSE,

  private = list(
    con = NULL,
    extract_ = NULL,
    transform_ = NULL,
    load_ = NULL,
    extract_test_ = NULL,
    transform_test_ = NULL,
    load_test_ = NULL,
    test_queries = NULL,
    extracted_data = NULL,
    transformed_data = NULL,
    log_table = NULL
  ),

  public = list(
    path = NULL,
    initialize = function(path, extract, extract_test, transform,
                          transform_test, load, load_test, test_queries,
                          db_name, rollback = NULL) {
      self$path <- path
      ## TODO: Only set up connection when it is actually needed
      private$con <- db_connect(db_name, path)
      private$extract_ <- extract
      private$extract_test_ <- extract_test
      private$transform_ <- transform
      private$transform_test_ <- transform_test
      private$load_ <- load
      private$load_test_ <- load_test
      private$test_queries <- test_queries
      private$log_table <- db_get_log_table(db_name, path)
      lockBinding("path", self)
    },

    rollback = function() {
    },

    extract = function() {
      message(sprintf("Running extract %s", self$path))
      private$extracted_data <- run_extract(private$con, private$extract_,
                                            self$path, private$extract_test_)
      invisible(private$extracted_data)
    },

    transform = function() {
      message(sprintf("Running transform %s", self$path))
      private$transformed_data <- run_transform(private$con, private$transform_,
                                                self$path, private$extracted_data,
                                                private$transform_test_)
      invisible(private$transformed_data)
    },

    load = function(comment = NULL, dry_run = FALSE, force = FALSE) {
      message(sprintf("Running load %s", self$path))
      if (!force && !dry_run && !git_repo_is_clean(self$path)) {
        stop("Can't run load as repository has unstaged changes. Update git or run in dry-run mode.")
      }
      run_load(private$con, private$load_, private$transformed_data,
               private$test_queries, self$path, private$load_test_, dry_run,
               private$log_table, comment)
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
    },

    get_log_table = function() {
      private$log_table
    }
  )
)
