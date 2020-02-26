load <- function(transformed_data, con) {
  stop("Throwing an error during a transaction")
  for (name in names(transformed_data)) {
    DBI::dbWriteTable(con, name, transformed_data[[name]], append = TRUE)
  }
}
