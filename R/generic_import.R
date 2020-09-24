#' Generic import object.
#'
#' This shouldn't be initialised directly - this object manages transactions
#' and the db connection and should be extended to implement actual imports.
Import <- R6::R6Class(
  "Import",
  cloneable = FALSE,

  private = list(
    db_name = NULL,
    con = NULL,
    transaction_active = FALSE,
    modify_in_transaction = TRUE,
    log_table = NULL,

    ## Import options
    mode = NULL,
    import_config = NULL,
    repo_config = NULL,
    confirm = NULL,
    require_branch = NULL,

    has_pre_load = FALSE,
    has_post_load = FALSE,
    log = NULL,

    #' @description
    #' Tidy up open connections, rolling back any active transactions
    close_connection = function() {
      if (private$transaction_active) {
        message("Rolling back active transaction")
        self$rollback_transaction()
      }
      ## Connection will always be null on first call to reload
      if (!is.null(private$con)) {
        tryCatch(
          DBI::dbDisconnect(private$con),
          error = function(e) {
            message("While disconnecting from db, ignored error:\n", e$message)
          }
        )
      }
    },

    pre_load = function() {
      NULL
    },

    post_load = function() {
      NULL
    },

    test_queries = function() {
      NULL
    },

    import = function() {
      NULL
    },

    test_import = function(before, after) {
      NULL
    }
  ),

  public = list(
    #' @field path Path to directory containing import object
    path = NULL,

    #' @description
    #' Create Import object - shouldn't be called directly. Use
    #' \code{\link{dettl}} to create the object
    #' @param path Path to directory containing import object
    #' @param db_name Database from dettl config to create import object for
    #' @return A new `Import` object
    initialize = function(path, db_name) {
      self$path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      private$db_name <- db_name
    },

    #' @description
    #' Reload the objects sources to refresh source code or repair a broken
    #' Postgres connection.
    reload = function() {
      private$import_config <- read_config(self$path)
      private$modify_in_transaction <- private$import_config$dettl$transaction
      private$repo_config <- dettl_config(self$path)

      private$db_name <- private$db_name %||%
        get_default_type(private$repo_config)
      private$close_connection()
      private$con <- db_connect(private$db_name, self$path)

      private$log_table <- db_get_log_table(private$db_name, self$path)
      private$require_branch <-
        private$repo_config$db[[private$db_name]]$require_branch

      private$mode <- private$import_config$dettl$mode

      private$confirm <- private$repo_config$db[[private$db_name]]$confirm
    },

    #' @description
    #' Run checks that db can be modified, this checks that:
    #' * If require_branch set in cfg that import is for that branch
    #' * If confirm TRUE asks users to confirm action will modify db
    #' * If git is dirty, checks that user has explicitly said that is okay
    #' @param dry_run Whether to run in dry run mode. If TRUE then any database
    #' changes will be rolled back. Defaults to FALSE.
    #' @param allow_dirty_git If TRUE then skips check that the import is up to
    #' date with remote git repo. FALSE by default.
    pre_modify_checks = function(dry_run, allow_dirty_git) {
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
      if (!allow_dirty_git && !dry_run && !git_repo_is_clean(self$path)) {
        stop("Can't run as repository has unstaged changes. Update git or run in dry-run mode.")
      }

      invisible(TRUE)
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
    #' Get the database connection being used by the import. Used for testing.
    #' @return The DBI connection
    get_connection = function() {
      private$con
    },

    #' @description
    #' Get the name of the log table for a particular import. This is the name
    #' of the log table configured in the dettl config
    #' @return Log table name
    get_log_table = function() {
      private$log_table
    },

    #' @description
    #' Start a transaction
    begin_transaction = function() {
      DBI::dbBegin(private$con)
      private$transaction_active <- TRUE
    },

    #' @description
    #' Rollback a transaction
    rollback_transaction = function() {
      DBI::dbRollback(private$con)
      private$transaction_active <- FALSE
    },

    #' @description
    #' Commit a transaction
    commit_transaction = function() {
      DBI::dbCommit(private$con)
      private$transaction_active <- FALSE
    },

    #' @description
    #' Run the load step ensuring tests pass before db changes are committed.
    #'
    #' Runs the load function on the DB within a transaction. Then run a set of
    #' tests on the DB and rollback the changes if any should fail.
    #'
    #' @param comment An optional comment to add to the import log table for this
    #' run.
    #'
    #' @keywords internal
    #'
    do_import <- function(comment) {
      self$log <- ImportLog$new(private$con, private$log_table, private$path,
                                comment)
      self$log$start_timer()
      withr::with_dir(private$path, {
        message("\t- Running test queries before making any changes")
        before <- private$test_queries()
        if (isTRUE(has_pre_load)) {
          message("\t- Running pre-load")
          private$pre_load()
        }
        message("\t- Running load step")
        private$import()
        if (isTRUE(has_post_load)) {
          message("\t- Running post-load")
          private$post_load()
        }
        message("\t- Running test queries after making changes")
        after <- private$test_queries()
        message(sprintf("\t- Running load tests %s", test_file))
        test_results <- private$test_import(before, after)
        if (!all_passed(test_results)) {
          stop("Failed to load data - not all tests passed.")
        }
      })
      self$log$stop_timer()
      invisible(TRUE)
    }
  )
)


