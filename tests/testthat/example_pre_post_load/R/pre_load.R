pre_load <- function(transformed_data, con) {
  DBI::dbWriteTable(con, "people",
                    data.frame(name = "Ed", age = 5, height = 75,
                               stringsAsFactors = FALSE),
                    append = TRUE)
}
