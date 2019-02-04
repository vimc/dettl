load <- function(transformed_data, con) {
  "Executed load function"
}

test_query <- function(transformed_data, con) {
  values <- list()
  values$count <-  DBI::dbGetQuery(con, "SELECT count(*) from people")[1,1]
  values
}


