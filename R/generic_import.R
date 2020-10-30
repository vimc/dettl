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

    pre_load = function() {
      NULL
    },

    post_load = function() {
      NULL
    },

    test_queries = function() {
      NULL
    },

    do_load = function() {
      NULL
    },

    test_load = function(before, after) {
      NULL
    },

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
    }
  ),

  public = list(
    #' @field path Path to directory containing import object
    path = NULL,

    #' @description
    #' Get the database connection being used by the import. Used for testing.
    #' @return The DBI connection
    get_connection = function() {
      private$con
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
    #' Get the name of the log table for a particular import. This is the name
    #' of the log table configured in the dettl config
    #' @return Log table name
    get_log_table = function() {
      private$log_table
    },

    #' @description
    #' Create Import object - shouldn't be called directly. Use
    #' \code{\link{dettl}} to create the object
    #' @param path Path to directory containing import object
    #' @param db_name Database from dettl config to create import object for
    #' @return A new `Import` object
    initialize = function(path, db_name) {
      self$path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      private$db_name <- db_name
      self$reload()
    },

    #' @description
    #' Reload the objects sources to refresh source code or repair a broken
    #' Postgres connection.
    reload = function() {
      self$read_config()
      private$modify_in_transaction <- private$import_config$dettl$transaction
      private$repo_config <- dettl_config(self$path)

      private$db_name <- private$db_name %||%
        get_default_type(private$repo_config)
      private$close_connection()
      private$con <- db_connect(private$db_name, self$path)

      private$log_table <- db_get_log_table(private$db_name, self$path)
      private$log <- ImportLog$new(private$con, private$log_table, self$path)
      private$require_branch <-
        private$repo_config$db[[private$db_name]]$require_branch

      private$mode <- private$import_config$dettl$mode

      private$confirm <- private$repo_config$db[[private$db_name]]$confirm
    },

    #' @description
    #' Abstract impl should be overridden by subclass
    read_config = function(path) {
      NULL
    },

    #' @description
    #' Run the extract stage of the data import - does nothing for generic
    #' override in subclass if required.
    extract = function() {
      message("No extract function defined for this import, skipping step")
      invisible(NULL)
    },

    #' @description
    #' Run the extract stage of the data import - does nothing for generic
    #' override in subclass if required.
    transform = function() {
      message("No transform function defined for this import, skipping step")
      invisible(NULL)
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
        question <- sprintf(
          "About to upload to database '%s' are you sure you want to proceed?",
          private$db_name)
        confirmed <- askYesNo(question, default = FALSE)
        if (is.na(confirmed) || !confirmed) {
          message("Not uploading to database.")
          return(invisible(FALSE))
        }
      }
      if (!allow_dirty_git && !dry_run && !git_repo_is_clean(self$path)) {
        stop(paste0("Can't run as repository has unstaged changes. Update ",
                    "git or run in dry-run mode."))
      }
      private$log$verify_first_run()
      invisible(TRUE)
    },

    #' @description
    #' Run the import step ensuring tests pass before db changes are committed.
    #'
    #' Runs the import on the DB within a transaction. Then run a set of
    #' tests on the DB and rollback the changes if any should fail.
    #'
    #' @param comment An optional comment to add to the import log table for this
    #' run.
    #' @param dry_run Whether to run in dry run mode. If TRUE then any database
    #' changes will be rolled back. Defaults to FALSE.
    #' @param allow_dirty_git If TRUE then skips check that the import is up to
    #' date with remote git repo. FALSE by default.
    #'
    #' @keywords internal
    #'
    load = function(comment = NULL, dry_run = FALSE,
                    allow_dirty_git = FALSE) {
      message(sprintf("Running load %s", self$path))
      use_transaction <- private$modify_in_transaction || dry_run
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
        checks_passed <- self$pre_modify_checks(dry_run, allow_dirty_git)
        if (isFALSE(checks_passed)) {
          return(invisible(FALSE))
        }
        private$log$set_comment(comment)
        private$log$start_timer()
        withr::with_dir(self$path, {
          message("\t- Running test queries before making any changes")
          before <- private$test_queries()
          if (isTRUE(private$has_pre_load)) {
            message("\t- Running pre-load")
            private$pre_load()
          }
          message("\t- Running load step")
          private$do_load()
          if (isTRUE(private$has_post_load)) {
            message("\t- Running post-load")
            private$post_load()
          }
          message("\t- Running test queries after making changes")
          after <- private$test_queries()
          message(sprintf("\t- Running load tests %s", private$load_test_))
          test_results <- private$test_load(before, after)
          if (!all_passed(test_results)) {
            stop("Failed to load data - not all tests passed.")
          }
        })
        private$log$stop_timer()
        if (dry_run) {
          self$rollback_transaction()
          message("All tests passed, rolling back dry run import.")
        } else {
          message("All tests passed, commiting changes to database.")
          private$log$write_log()
          if (use_transaction) {
            self$commit_transaction()
          }
        }
      }, error = function(e) {
        if (use_transaction) {
          message("Rolling back changes to database as error has occured")
          self$rollback_transaction()
        } else {
          message(paste0("ATTENTION: even though your load has failed, because",
                         " you did not use a transaction, the database may ",
                         "have been modified"))
        }
      })
      invisible(TRUE)
    },

    #' @description
    #' For generic import this is just running the load step,
    #' expect this gets overridden in subclasses.
    #'.
    #' @param comment An optional comment to add to the import log table for this
    #' run.
    #' @param dry_run Whether to run in dry run mode. If TRUE then any database
    #' changes will be rolled back. Defaults to FALSE.
    #' @param allow_dirty_git If TRUE then skips check that the import is up to
    #' date with remote git repo. FALSE by default.
    #'
    #' @keywords internal
    #'
    run_import = function(comment = NULL, dry_run = FALSE,
                          allow_dirty_git = FALSE,
                          stage = c("extract", "transorm")) {
      if ("extract" %in% stage) {
        self$extract()
      }
      if ("transform" %in% stage) {
        self$transform()
      }
      if ("load" %in% stage) {
        self$load(comment, dry_run, allow_dirty_git)
      }
      invisible(self)
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
    }
  )
)
