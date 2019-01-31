load <- function(transformed_data, con) {
  DBI::dbWriteTable(con, "people", transformed_data$people, append = TRUE)
}