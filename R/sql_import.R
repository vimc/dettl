#' Manage SQL based data import.
#'
#' This object should not be initialised directly. Use \code{\link{dettl}} to
#' create the object.
#'
#' Import can be run by working with import object returned by
#' \code{\link{dettl}} or by running top-level functions. Run the import by
#' working with this object if you want to step through the import process
#' stage by stage and inspect the data after each stage.
#'
#' @examples
#' path <- dettl:::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl"))
#' import_path <- file.path(path, "person_information")
#'
#' import <- dettl::dettl(import_path, db_name = "test")
#' import$run()
#'
# nolint start
SqlImport <- R6::R6Class(
  # nolint end
  "SqlImport",
  inherit = Import,
  cloneable = FALSE,

  private = list(
    sql = NULL,
    load_ = NULL,
    load_pre_ = NULL,
    load_post_ = NULL,
    load_test_ = NULL,
    test_queries_ = NULL,

    pre_load = function() {
      private$load_pre_(private$transformed_data, private$con)
    },

    post_load = function() {
      private$load_post_(private$transformed_data, private$con)
    },

    test_queries = function() {
      private$test_queries_(private$con)
    },

    do_load = function() {
      private$load_(private$con)
    },

    test_load = function(before, after) {
      run_load_tests(private$load_test_, before, after, NULL, NULL, private$con)
    }
  ),

  public = list(

    #' @description
    #' Reload the objects sources to refresh source code or repair a broken
    #' Postgres connection.
    reload = function() {

      super$reload()

      private$load_ <- private$import_config$load$func
      private$load_pre_ <- private$import_config$load$pre
      if (!is.null(private$load_pre_)) {
        private$has_pre_load <- TRUE
      }
      private$load_post_ <- private$import_config$load$post
      if (!is.null(private$load_post_)) {
        private$has_post_load <- TRUE
      }
      private$load_test_ <- private$import_config$load$test
      private$test_queries_ <- private$import_config$load$verification_queries
    },

    #' @description
    #' Read and parse config from path.
    read_config = function() {
      private$import_config <- read_sql_config(self$path)
    }
  )
)
