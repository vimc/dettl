start_connection <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  people_schema <- setNames(c("character", "integer", "integer"), 
                            c("name", "age", "height"))
  DBI::dbCreateTable(con, "people", people_schema)
  con
}