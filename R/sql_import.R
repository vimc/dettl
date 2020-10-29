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
    test_queries = NULL,
    sql = NULL
  ),

  public = list(

    #' @description
    #' Reload the objects sources to refresh source code or repair a broken
    #' Postgres connection.
    reload = function() {

      super$reload()

      ## Source sql file as text?
      private$sql <- readSqlfromfile()

      private$test_queries <- private$import_config$load$verification_queries
    },

    #' @description
    #' Run the SQL import
    #' @param comment Optional comment which will be persisted in the log of
    #' the import run in the database.
    #' @param dry_run Whether to run in dry run mode. If TRUE then any database
    #' changes will be rolled back. Defaults to FALSE.
    #' @param allow_dirty_git If TRUE then skips check that the import is up to
    #' date with remote git repo. FALSE by default.
    run = function(comment = NULL, dry_run = FALSE, allow_dirty_git = FALSE) {
      checks_passed <- super$pre_modify_checks(dry_run, allow_dirty_git)
      if (isFALSE(checks_passed)) {
        return(invisible(FALSE))
      }
      message(sprintf("Running load %s", self$path))

      use_transaction <- private$modify_in_transaction || dry_run
      if (use_transaction) {
        self$begin_transaction()
      }
      message(
        sprintf("Running import %s:",
                if (use_transaction) {
                  "in a transaction"
                } else {
                  "not in a transaction"
                }))
      withCallingHandlers({
        log_data <- run_sql_import(private$con, private$sql,
                                   private$test_queries, private$load_test_,
                                   private$log_table, comment)
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
      output <- list(
        import = self
      )
      class(output) <- "import"
      output
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

run_sql_import <- function(con, sql, test_queries, test_file, log_table,
                           comment) {
  ## Run test queries
  ## Execute sql
  ## Run test queries
  ## Run test
  ## Create log data

  load <- function() {
    DBI::dbExecute(con, sql)
  }
  run_load(con, load, NULL, NULL, test_queries,
           NULL, NULL, path, test_file, log_table, comment)
}
