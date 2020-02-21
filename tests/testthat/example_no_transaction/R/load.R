load <- function(transformed_data, con) {
  transformed_data$people_load <- read.csv("data/people.csv",
                                           stringsAsFactors = FALSE)
  for (name in names(transformed_data)) {
    if (name == "people_load") {
      stop("Not importing table 'people_load'")
    } else {
      DBI::dbWriteTable(con, name, transformed_data[[name]], append = TRUE)
    }
  }
}
