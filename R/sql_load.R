get_sql_load <- function(path) {
  ## It is not very nice that we have to read & parse this file to send it to
  ## DB, ideally we could just execute the file directly on DB but DBI only
  ## allows sending individual queries atm see
  ## https://github.com/r-dbi/RPostgres/issues/213
  sql <- readLines(path)
  sql <- sql[!is_empty(sql)]
  statement <- paste(sql, collapse = "\n")
  statements <- unlist(strsplit(statement, ";", fixed = TRUE))
  function(con) {
    withCallingHandlers(
      lapply(statements, function(cmd) {
        DBI::dbExecute(con, cmd)
      }),
      error = function(e) {
        e$message <- paste0("SQL error:\n", e$message)
        stop(e)
      })
  }
}

is_empty <- function(x) {
  is.null(x) | is.na(x) | length(x) == 0 | trimws(x) == ""
}
