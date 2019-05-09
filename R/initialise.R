#' Initialise the database by creating log table if it doesn't already exist
#'
#' @param path Path to import directory containing db connection configuration.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the `dettl_config.yml`.
#'
#' @export
#' @examples
#' path <- dettl:::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl"),
#'   add_log_table = FALSE
#' )
#' dettl_initialise(file.path(path, "person_information"), "test")
#'
dettl_initialise <- function(path, db_name) {
  ## Get the connection info
  path <- normalizePath(path, mustWork = TRUE)
  con <- db_connect(db_name, path)
  on.exit(DBI::dbDisconnect(con))
  ## Create the table
  dialect <- sql_dialect(con)
  path <- switch(
    dialect,
    sqlite = system.file("sql", "sqlite", "create_log_table.sql",
                         package = "dettl"),
    postgres = system.file("sql", "postgresql", "create_log_table.sql",
                           package = "dettl"),
    stop(sprintf(
      "Can't initialise DB %s as not a SQLite or Postgres type. Got %s.",
       db_name, class(con)
    ))
  )
  query <- read_lines(path)
  message(sprintf("Creating log table in DB %s.", db_name))
  DBI::dbExecute(con, query)
}
