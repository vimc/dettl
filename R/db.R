#' Connect to the database configured via yaml.
#'
#' @param type The type of connection to make (\code{source},
#'   \code{destination}).
#' @param path Path to directory contianing yaml config.
#'
#' @keywords internal
#'
db_connect <- function(type, path) {
  config <- db_config(path)
  x <- dettl_db_args(type, config)
  con <- do.call(DBI::dbConnect,
                 c(list(x$driver()), resolve_secrets(x$args, config)))
  con
}

#' Get the DB args from config.
#'
#' Converts the configured DB driver to appropriate driver function and
#' map the args.
#'
#' @param type The db type to get the args for, source or destination.
#' @param config dettl_config object.
#'
#' @keywords internal
#'
dettl_db_args <- function(type, config) {
  x <- config[[type]]
  driver <- getExportedValue(x$driver[[1L]], x$driver[[2L]])

  ## TODO: Ensure args using vault secrets can be located.
  ## Add impl when needed - see orderly
  ## args <- resolve_driver_config(x$args, config)

  if (x$driver[[2]] == "SQLite") {
    dbname <- x$args$dbname
    if (is.null(dbname) || !nzchar(dbname) || tolower(dbname) == ":memory:") {
      stop("Cannot use a transient SQLite database with orderly")
    }
    if (is_relative_path(x$args$dbname)) {
      x$args$dbname <- file.path(config$path, x$args$dbname)
    }
  }

  list(driver = driver, args = x$args)
}
