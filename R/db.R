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
  con <- do.call(
    DBI::dbConnect,
    c(list(x$driver()), x$args)
  )
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
  if (sql_dialect(con) == "sqlite") {
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

  withr::with_envvar(
    envir_read(config$path),
    resolved_args <- vaultr::vault_resolve_secrets(
      x$args,
      addr = config$vault_server
    )
  )
  list(driver = driver, args = resolved_args, log_table = x$log_table)
}

#' Verify the data adhered to the DB schema.
#'
#' Ensure that table is in the DB and has only columns which exist in the DB.
#' And optionally check that table contains exactly the columns in the DB.
#'
#' @param con The active DB connection to check the schema for.
#' @param table_name The name of the table to check.
#' @param table The table to check.
#' @param context_info Info to be logged should a check fail.
#' @param solution_text Text describing possible solution should error occur.
#'
#' @keywords internal
#'
verify_table <- function(con, table_name, table, identical_columns = FALSE,
                         context_info = "", solution_text = "") {
  if (!DBI::dbExistsTable(con, table_name)) {
    stop(sprintf(
      "%s: Table '%s' is missing from db schema. %s",
      context_info, table_name, solution_text
    ))
  }
  col_names <- DBI::dbListFields(con, table_name)
  for (col_name in colnames(table)) {
    if (!(col_name %in% col_names)) {
      stop(sprintf(
        "%s: Column '%s' in table '%s' but is missing from db schema.",
        context_info, col_name, table_name
      ))
    }
  }
  if (identical_columns) {
    for(col_name in col_names) {
      if (!(col_name %in% colnames(table))) {
        stop(sprintf(
          "%s: Column '%s' in table '%s' in DB but is missing from local table.",
          context_info, col_name, table_name))
      }
    }
  }
}


#' Parse date into POSIXct UTC from SQL db.
#'
#' Parses the date according to the SQL driver used. Expects that if
#' driver is Postgres this will already be POSIXct otherwise if SQLite
#' driver converts to POSIXct.
#'
#' @param con Active db connection.
#' @param date The date.
#'
#' @return The parsed date
#'
#' @keywords internal
parse_sql_date <- function(con, date) {
  if (sql_dialect(con) == "sqlite") {
    date <- as.POSIXct(date, origin = "1970-01-01", tz = "UTC")
  }
  date
}

#' Get the SQL dialect used for a connection.
#'
#' @param con The connection to test.
#'
#' @return The dialect used, either "sqlite" or "postgres".
#'
#' @keywords internal
sql_dialect <- function(con) {
  dialect <- NULL
  if (inherits(con, "SQLiteConnection")) {
    dialect <- "sqlite"
  } else if (inherits(con, "PqConnection")) {
    dialect <- "postgresql"
  }
  dialect
}
