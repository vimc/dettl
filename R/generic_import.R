Import <- R6::R6Class(
  "Import",
  cloneable = FALSE,

  private = list(
    con = NULL,
    transaction_active = FALSE,
    db_name = NULL,
    load_in_transaction = TRUE,
    require_branch = NULL,
    log_table = NULL,
    import_config = NULL,
    repo_config = NULL,

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
      private$load_in_transaction <- private$import_config$dettl$transaction
      private$repo_config <- dettl_config(self$path)

      private$db_name <- private$db_name %||%
        get_default_type(private$repo_config)
      private$close_connection()
      private$con <- db_connect(private$db_name, self$path)

      private$log_table <- db_get_log_table(private$db_name, self$path)
      private$require_branch <-
        private$repo_config$db[[private$db_name]]$require_branch
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
    }
  )
)
