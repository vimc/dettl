log_import <- function(con, log_table, name, comment) {
  if (is.null(comment)) {
    comment <- ""
  }
  import_log <- data.frame(name = name, date = Sys.time(), comment = comment,
                           stringsAsFactors = FALSE)
  query <- DBI::dbAppendTable(con, log_table, import_log)
}
