get_sql_load <- function(path) {
  ## It is not very nice that we have to read & parse this file to send it to
  ## DB, ideally we could just execute the file directly on DB but DBI only
  ## allows sending individual queries atm see
  ## https://github.com/r-dbi/RPostgres/issues/213
  statements <- parse_sql(path)
  function(con) {
    withCallingHandlers(
      for (cmd in statements) {
        DBI::dbExecute(con, cmd)
      },
      error = function(e) {
        e$message <- paste0("SQL error:\n", e$message)
        stop(e)
      })
  }
}

parse_sql <- function(path) {
  sql <- readLines(path)
  sql <- sql[nzchar(trimws(sql))]
  statement <- paste(sql, collapse = "\n")
  unlist(strsplit(statement, ";[\n]*"))
}
