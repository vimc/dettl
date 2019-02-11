load <- function(transformed_data, con) {
  for (name in names(transformed_data)) {
    DBI::dbWriteTable(con, name, transformed_data[[name]], append = TRUE)
  }
}
