get_sql_load <- function(path) {
  statement <- paste(readLines(path), collapse = "\n")
  function(con) {
    withCallingHandlers(
      DBI::dbExecute(con, statement),
      error = function(e) {
        e$message <- paste0("SQL error:\n", e$message)
        stop(e)
      })
  }
}
