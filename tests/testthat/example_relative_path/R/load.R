load <- function(transformed_data, con) {
  transformed_data$people_load <- read.csv("data/people.csv",
                                       stringsAsFactors = FALSE)
  for (name in names(transformed_data)) {
    DBI::dbWriteTable(con, name, transformed_data[[name]], append = TRUE)
  }
}
