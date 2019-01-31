create_test_db <- function(db_name) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_name)
  people_schema <- setNames(c("character", "integer", "integer"),
                            c("name", "age", "height"))
  DBI::dbCreateTable(con, "people", people_schema)
  DBI::dbDisconnect(con)
}
