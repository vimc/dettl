test_queries <- function(con) {
  values <- list()
  values$people_count <- DBI::dbGetQuery(con, "SELECT count(*) from people")[1, 1]
  values$jobs_count <- DBI::dbGetQuery(con, "SELECT count(*) from jobs")[1, 1]
  values
}

