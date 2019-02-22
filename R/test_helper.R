## Create a simple "people" table in the DB for testing the import process.
prepare_example_db <- function(db_name, dir = getwd()) {
  path <- file.path(dir, db_name)
  if (file.exists(path)) {
    ## Ensure we always start with a fresh DB
    file.remove(path)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  people_schema <- stats::setNames(c("character", "integer", "integer"),
                                   c("name", "age", "height"))
  DBI::dbCreateTable(con, "people", people_schema)
  DBI::dbDisconnect(con)
  path
}

