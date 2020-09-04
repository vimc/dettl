post_load <- function(transformed_data, con) {
  DBI::dbExecute(con, "CREATE INDEX people_name ON people(name)")
}
