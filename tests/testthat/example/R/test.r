test <- function(transformed_data, con) {
  count_before <- DBI::dbGetQuery(con, 
                                  "SELECT count(*) from people")[1,1]
  DBI::dbBegin(con)
  DBI::dbWriteTable(con, "people", transformed_data$people, append = TRUE)
  count_after <- DBI::dbGetQuery(con, 
                                  "SELECT count(*) from people")[1,1]
  DBI::dbRollback(con)
  if (count_before + 2 == count_after) {
    message("Tests passed - 2 rows have been added")
  } else {
    stop(sprintf("Wrong number of rows added, expected %i got %i", 
                 count_before + 2, count_after))
  }
}