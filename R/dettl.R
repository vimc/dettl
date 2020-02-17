#' Manage data import.
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
#' path <- dettl::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl"))
#' import_path <- file.path(path, "person_information")
#'
#' import <- dettl::dettl(import_path, db_name = "test")
#' import$extract()
#' import$transform()
#' import$load()
#'

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
    extract_passed = FALSE,
    transform_passed = FALSE,
    load_test_ = NULL,
    test_queries = NULL,
    extracted_data = NULL,
    transformed_data = NULL,
    log_table = NULL,
    confirm = NULL,
    require_branch = NULL,
    db_name = NULL,
    mode = NULL
  ),

  public = list(
    #' @field path Path to directory containing import object
    path = NULL,

    #' @description
    #' Create DataImport object - shouldn't be called directly. Use
    #' \code{\link{dettl}} to create the object
    #' @param path Path to directory containing import object
    #' @param db_name Database from dettl config to create import object for
    #' @return A new `DataImport` object
    initialize = function(path, db_name) {
      self$path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      private$db_name <- db_name
      lockBinding("path", self)
      self$reload()
    },

    #' @description
    #' Reload the objects sources to refresh source code or repair a broken
    #' Postgres connection.
    reload = function() {
      dettl_config <- read_config(self$path)
      private$mode <- check_valid_mode(dettl_config$dettl$mode)
      if (dettl_config$load$automatic) {
        load_func <- get_auto_load_function(private$mode)
      } else {
        load_func <- dettl_config$load$func
      }
      cfg <- dettl_config(self$path)

      db_name <- private$db_name %||% get_default_type(cfg)
      private$con <- db_connect(db_name, self$path)
      private$extract_ <- dettl_config$extract$func
      private$extract_test_ <- dettl_config$extract$test
      private$transform_ <- dettl_config$transform$func
      private$transform_test_ <- dettl_config$transform$test
      private$load_ <- load_func
      private$load_test_ <- dettl_config$load$test
      private$test_queries <- dettl_config$load$verification_queries

      private$log_table <- db_get_log_table(db_name, self$path)
      private$confirm <- cfg$db[[db_name]]$confirm
      private$require_branch <- cfg$db[[db_name]]$require_branch
    },

    #' @description
    #' Custom formatter for pretty printing object summary.
    #' @param brief If TRUE then print a brief summary.
    format = function(brief = FALSE) {
      data_import_format(self, brief, class(self)[[1L]])
    },

    #' @description
    #' Print help page for the object
    help = function() {
      utils::help(class(self)[[1L]], package = "dettl")
    },

    #' @description
    #' Run the extract stage of the data import
    extract = function() {
      message(sprintf("Running extract %s", self$path))
      private$extracted_data <- run_extract(private$con, private$extract_,
                                            self$path)
      private$extract_passed <- test_extract(private$con, self$path,
                                             private$extract_test_,
                                             private$extracted_data)
      invisible(private$extracted_data)
    },

    #' @description
    #' Run the transform stage of the data import
    transform = function() {
      message(sprintf("Running transform %s", self$path))
      private$transformed_data <- run_transform(private$transform_,
                                                private$extracted_data,
                                                private$extract_passed)
      private$transform_passed <- test_transform(private$con, self$path,
                                                 private$mode,
                                                 private$transform_test_,
                                                 private$transformed_data,
                                                 private$extracted_data)
      invisible(private$transformed_data)
    },

    #' @description
    #' Run the load stage of the data import
    #' @param comment Optional comment which will be persisted in the log of
    #' the import run in the database.
    #' @param dry_run Whether to run in dry run mode. If TRUE then any database
    #' changes will be rolled back. Defaults to FALSE.
    #' @param force If TRUE then checks that repo is up to date with git remote
    #' will be skipped. Defautls to FALSE.
    load = function(comment = NULL, dry_run = FALSE, force = FALSE) {
      if (is.null(private$transformed_data)) {
        stop("Cannot run load as no data has been transformed.")
      }
      if (!private$transform_passed) {
        stop("Cannot run load as transform tests failed.")
      }
      if (!is.null(private$require_branch)) {
        if (git_branch(self$path) != private$require_branch) {
          stop(sprintf("This import can only be run from the '%s' branch",
                       private$require_branch), call. = FALSE)
        }
      }
      if (private$confirm) {
        confirmed <- askYesNo(
          sprintf(
            "About to upload to database '%s' are you sure you want to proceed?",
            private$db_name),
          default = FALSE)
        if (is.na(confirmed) || !confirmed) {
          message("Not uploading to database.")
          return(invisible(FALSE))
        }
      }
      message(sprintf("Running load %s", self$path))
      if (!force && !dry_run && !git_repo_is_clean(self$path)) {
        stop("Can't run load as repository has unstaged changes. Update git or run in dry-run mode.")
      }
      run_load(private$con, private$load_, private$extracted_data, private$transformed_data,
               private$test_queries, self$path, private$load_test_, dry_run,
               private$log_table, comment)
      invisible(TRUE)
    },

    #' @description
    #' Get the database connection being used by the import. Used for testing.
    #' @return The DBI connection
    get_connection = function() {
      private$con
    },

    #' @description
    #' Get the extracted data created by the extract step
    #' @return The extracted data
    get_extracted_data = function() {
      private$extracted_data
    },

    #' @description
    #' Get the transformed data created by the transform step
    #' @return The transformed data
    get_transformed_data = function() {
      private$transformed_data
    },

    #' @description
    #' Get the name of the log table for a particular import. This is the name
    #' of the log table configured in the dettl config
    #' @return Log table name
    get_log_table = function() {
      private$log_table
    }
  )
)
