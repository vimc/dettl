#' Manage R based data import.
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
#' import$extract()
#' import$transform()
#' import$load()
#'
# nolint start
RImport <- R6::R6Class(
# nolint end
  "RImport",
  inherit = Import,
  cloneable = FALSE,

  private = list(
    extract_ = NULL,
    transform_ = NULL,
    load_ = NULL,
    load_pre_ = NULL,
    load_post_ = NULL,
    extract_test_ = NULL,
    transform_test_ = NULL,
    extract_passed = FALSE,
    transform_passed = FALSE,
    load_test_ = NULL,
    test_queries = NULL,
    extracted_data = NULL,
    transformed_data = NULL,
    confirm = NULL,
    mode = NULL,

    invalidate_extracted_data = function() {
      private$extracted_data <- NULL
      private$extract_passed <- FALSE
    },

    invalidate_transformed_data = function() {
      private$transformed_data <- NULL
      private$transform_passed <- FALSE
    }
  ),

  public = list(

    #' @description
    #' Create RImport object - shouldn't be called directly. Use
    #' \code{\link{dettl}} to create the object
    #' @param path Path to directory containing import object
    #' @param db_name Database from dettl config to create import object for
    #' @return A new `RImport` object
    initialize = function(path, db_name) {
      super$initialize(path, db_name)
      self$reload()
    },

    #' @description
    #' Reload the objects sources to refresh source code or repair a broken
    #' Postgres connection.
    reload = function() {
      private$invalidate_extracted_data()
      private$invalidate_transformed_data()

      super$reload()

      private$mode <- private$import_config$dettl$mode
      private$confirm <- private$repo_config$db[[private$db_name]]$confirm

      if (private$import_config$load$automatic) {
        load_func <- get_auto_load_function(private$mode)
      } else {
        load_func <- private$import_config$load$func
      }
      private$extract_ <- private$import_config$extract$func
      private$extract_test_ <- private$import_config$extract$test
      private$transform_ <- private$import_config$transform$func
      private$transform_test_ <- private$import_config$transform$test
      private$load_ <- load_func
      private$load_pre_ <- private$import_config$load$pre
      private$load_post_ <- private$import_config$load$post
      private$load_test_ <- private$import_config$load$test
      private$test_queries <- private$import_config$load$verification_queries
      lockBinding("path", self)
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
      private$invalidate_transformed_data()
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
                                                private$extract_passed,
                                                self$path)
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
    #' @param allow_dirty_git If TRUE then skips check that the import is up to
    #' date with remote git repo. FALSE by default.
    load = function(comment = NULL, dry_run = FALSE, allow_dirty_git = FALSE) {
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
      if (!allow_dirty_git && !dry_run && !git_repo_is_clean(self$path)) {
        stop("Can't run load as repository has unstaged changes. Update git or run in dry-run mode.")
      }

      use_transaction <- private$load_in_transaction || dry_run
      if (use_transaction) {
        self$begin_transaction()
      }
      message(
        sprintf("Running load %s:",
                if (use_transaction) {
                  "in a transaction"
                } else {
                  "not in a transaction"
                }))
      withCallingHandlers({
        log_data <- run_load(private$con, private$load_, private$extracted_data,
                             private$transformed_data, private$test_queries,
                             private$load_pre_, private$load_post_, self$path,
                             private$load_test_, private$log_table, comment)
        if (dry_run) {
          self$rollback_transaction()
          message("All tests passed, rolling back dry run import.")
        } else {
          message("All tests passed, commiting changes to database.")
          write_log(private$con, private$log_table, log_data)
          if (use_transaction) {
            self$commit_transaction()
          }
        }
      }, error = function(e) {
        if (use_transaction) {
          message("Rolling back changes to database as error has occured")
          self$rollback_transaction()
        } else {
          message("ATTENTION: even though your load has failed, because you did not use a transaction, the database may have been modified")
        }
      })
      invisible(TRUE)
    },

    #' @description
    #' Run multiple stages of the data import
    #' @param stage The stage or stages of the import to be run.
    #' @param comment Optional comment to be written to db log table when
    #' import is run.
    #' @param save Path and name to save data from each stage at, if TRUE then
    #' will save to a tempfile.
    #' @param dry_run If TRUE then any changes to the database will be rolled
    #' back.
    #' @param allow_dirty_git If TRUE then skips check that the import is up
    #' to date
    run = function(stage = c("extract", "transform"),
                   comment = NULL, save = FALSE,
                   dry_run = FALSE, allow_dirty_git = FALSE) {
      if ("extract" %in% stage) {
        self$extract()
      }
      if ("transform" %in% stage) {
        if (is.null(self$get_extracted_data())) {
          self$extract()
        }
        self$transform()
      }
      if ("load" %in% stage) {
        if (is.null(self$get_transformed_data())) {
          stop("Can't run load as transform stage has not been run.")
        }
        self$load(comment, dry_run, allow_dirty_git)
      }

      if (!isFALSE(save)) {
        if (isTRUE(save)) {
          save <- tempfile(fileext = ".xlsx")
        }
        dettl_save(self, save, stage)
      }

      output <- list(
        import = self,
        data = list(
          extract = self$get_extracted_data(),
          transform = self$get_transformed_data()
        )
      )
      class(output) <- "import"
      output
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
    }
  )
)
