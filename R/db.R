#' Connect to the database configured via yaml.
#'
#' @param type The db to connect to, must match a db configured in db config.
#' @param path Path to directory contianing yaml config.
#'
#' @keywords internal
#'
db_connect <- function(type, path) {
  x <- dettl_db_args(type, path)
  con <- do.call(DBI::dbConnect,
                 c(list(x$driver()), x$args))
  con
}

#' Get the DB args from config.
#'
#' Converts the configured DB driver to appropriate driver function and
#' map the args.
#'
#' @param type The db type to get the args for.
#' @param path Path to db config.
#'
#' @keywords internal
#'
dettl_db_args <- function(type, path) {
  config <- db_config(path)
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
  list(driver = driver, args = resolved_args)
}
