post_load <- function(transformed_data, con) {
  DBI::dbGetQuery(con, "CREATE INDEX people_name ON people(name)")
}
