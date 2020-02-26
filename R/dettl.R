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
    extract_ = NULL,
    transform_ = NULL,
    load_ = NULL,
    load_pre_ = NULL,
    load_post_ = NULL,
    extract_test_ = NULL,
    transform_test_ = NULL,
    load_test_ = NULL,
    test_queries = NULL,
    extracted_data = NULL,
    transformed_data = NULL,
    log_table = NULL,
    confirm = NULL,
    require_branch = NULL,
    db_name = NULL,
    mode = NULL,
    load_in_transaction = TRUE
  ),

  public = list(
    #' @field connection Connection object which manages transactions
    connection = NULL,
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
      self$connection <- Connection$new(self$path)
      lockBinding("path", self)
      self$reload()
    },

    #' @description
    #' Cleanup DataImport object - is called by garbage collector
    finalize = function() {
      self$connection$finalize()
    },

    #' @description
    #' Reload the objects sources to refresh source code or repair a broken
    #' Postgres connection.
    reload = function() {
      dettl_config <- read_config(self$path)
      private$mode <- dettl_config$dettl$mode
      private$load_in_transaction <- dettl_config$dettl$transaction
      if (dettl_config$load$automatic) {
        load_func <- get_auto_load_function(private$mode)
      } else {
        load_func <- dettl_config$load$func
      }
      cfg <- dettl_config(self$path)

      private$db_name <- private$db_name %||% get_default_type(cfg)
      self$connection$reset_connection(private$db_name)
      private$extract_ <- dettl_config$extract$func
      private$extract_test_ <- dettl_config$extract$test
      private$transform_ <- dettl_config$transform$func
      private$transform_test_ <- dettl_config$transform$test
      private$load_ <- load_func
      private$load_pre_ <- dettl_config$load$pre
      private$load_post_ <- dettl_config$load$post
      private$load_test_ <- dettl_config$load$test
      private$test_queries <- dettl_config$load$verification_queries

      private$log_table <- db_get_log_table(private$db_name, self$path)
      private$confirm <- cfg$db[[private$db_name]]$confirm
      private$require_branch <- cfg$db[[private$db_name]]$require_branch
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
      private$extracted_data <- run_extract(self$connection$con,
                                            private$extract_,
                                            self$path, private$extract_test_)
      invisible(private$extracted_data)
    },

    #' @description
    #' Run the transform stage of the data import
    transform = function() {
      message(sprintf("Running transform %s", self$path))
      private$transformed_data <- run_transform(self$connection$con,
                                                private$transform_,
                                                self$path,
                                                private$extracted_data,
                                                private$transform_test_,
                                                private$mode)
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
      run_load(self$connection, private$load_, private$extracted_data,
               private$transformed_data, private$test_queries,
               private$load_pre_, private$load_post_, self$path,
               private$load_test_, private$load_in_transaction, dry_run,
               private$log_table, comment)
      invisible(TRUE)
    },

    #' @description
    #' Get the database connection being used by the import. Used for testing.
    #' @return The DBI connection
    get_connection = function() {
      self$connection$con
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

Connection <- R6::R6Class(
  "Connection",
  cloneable = FALSE,

  private = list(
    transaction_active = FALSE,

    #' @description
    #' Tidy up open connections, rolling back any active transactions
    close_connection = function() {
      if (private$transaction_active) {
        warning("Rolling back active transaction")
        self$rollback()
      }
      ## Connection will always be null on first call to reload
      if (!is.null(self$con)) {
        tryCatch(
          DBI::dbDisconnect(self$con),
          error = function(e) {
            ## Consume the error and print as warning
            warning(e$message)
          }
        )
      }
      invisible(TRUE)
    }
  ),

  public = list(
    #' @field con Current connection being managed
    con = NULL,
    #' @field path Path to directory containing db config
    path = NULL,

    #' @description
    #' Create Connection object.
    initialize = function(path) {
      self$path <- path
    },

    #' @description
    #' Cleanup Connection object - is called by garbage collector
    finalize = function() {
      private$close_connection()
    },

    #' @description
    #' Reset the connection, clears old connection and creates a new one
    #' @param db_name Name of db in cfg to connect to
    reset_connection = function(db_name) {
      private$close_connection()
      self$con <- db_connect(db_name, self$path)
    },

    #' @description
    #' Start a transaction
    begin = function() {
      DBI::dbBegin(self$con)
      private$transaction_active <- TRUE
    },

    #' @description
    #' Rollback a transaction
    rollback = function() {
      DBI::dbRollback(self$con)
      private$transaction_active <- FALSE
    },

    #' @description
    #' Commit a transaction
    commit = function() {
      DBI::dbCommit(self$con)
      private$transaction_active <- FALSE
    }
  )
)
