test_queries <- function(con) {
  values <- list()
  values$count <-  DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1]
  values
}
