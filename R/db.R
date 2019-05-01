#' Connect to the database configured via yaml.
#'
#' Uses \code{\link[DBI]{dbConnect}} to connect to a DBMS. If this uses
#' \code{\link[RSQLite]{SQLite}} then it will ensure foreign key constraints are
#' enabled.
#'
#' @param type The db to connect to, must match a db configured in db config.
#' @param path Path to directory contianing yaml config.
#'
#' @keywords internal
#'
db_connect <- function(type, path) {
  x <- dettl_db_args(path, type)
  con <- do.call(DBI::dbConnect,
                 c(list(x$driver()), x$args))
  sqlite_enable_fk(con)
  con
}

#' Get the log table name from configuration for a particular db
#'
#' @param type The db to get the log table for, must match a db configured in
#' db config.
#' @param path Path to directory contianing yaml config.
#'
#' @keywords internal
#'
db_get_log_table <- function(type, path) {
  x <- dettl_db_args(path, type)
  x$log_table
}

#' Enable foreign key constraints for SQLite connections
#'
#' Foreign key constraints aren't enabled by default in SQLite. Ensure
#' they are enabled each time we connect to the db.
#'
#' @keywords internal
sqlite_enable_fk <- function(con) {
  if (inherits(con, "SQLiteConnection")) {
    DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  }
}

#' Get the DB args from config.
#'
#' Converts the configured DB driver to appropriate driver function and
#' map the args.
#'
#' @param path Path to db config.
#' @param type The db type to get the args for, if null defaults to the first
#' configured database.
#'
#' @keywords internal
#'
dettl_db_args <- function(path, type = NULL) {
  config <- dettl_config(path)
  if (is.null(type)) {
    type <- names(config$db)[[1]]
  }
  x <- config$db[[type]]
  if (is.null(x)) {
    stop(sprintf("Cannot find config for database %s.", type))
  }
  driver <- getExportedValue(x$driver[[1L]], x$driver[[2L]])

  if (x$driver[[2]] == "SQLite") {
    dbname <- x$args$dbname
    if (is.null(dbname) || !nzchar(dbname) || tolower(dbname) == ":memory:") {
      stop("Cannot use a transient SQLite database with dettl")
    }
    if (is_relative_path(x$args$dbname)) {
      x$args$dbname <- file.path(config$path, x$args$dbname)
    }
  }

  withr::with_envvar(envir_read(config$path),
    resolved_args <- vaultr::vault_resolve_secrets(x$args,
                                                   addr = config$vault_server)
  )
  list(driver = driver, args = resolved_args, log_table = x$log_table)
}
