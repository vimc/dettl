## Create a simple "people" table in the DB for testing the import process.
prepare_example_db <- function(db_name, dir = getwd()) {
  path <- file.path(dir, db_name)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  if (DBI::dbExistsTable(con, "people")) {
    DBI::dbRemoveTable(con, "people")
  }
  people_schema <- stats::setNames(c("character", "integer", "integer"),
                                   c("name", "age", "height"))
  DBI::dbCreateTable(con, "people", people_schema)
  DBI::dbDisconnect(con)
  path
}

