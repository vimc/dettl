log_import <- function(con, log_table, log_data) {
  query <- DBI::dbAppendTable(con, log_table, log_data)
}

get_log_data <- function(con, log_table, import_path, comment) {
  if (is.null(comment)) {
    comment <- ""
  }
  date <- Sys.time()
  ## Convert date to UTC for persitence
  attr(date, "tzone") <- "UTC"
  import_log <- data.frame(name = basename(import_path),
                           date = date,
                           comment = comment,
                           git_user = git_user(import_path),
                           git_email = git_email(import_path),
                           git_branch = git_branch(import_path),
                           git_hash = git_hash(import_path),
                           stringsAsFactors = FALSE)
}

verify_log_table <- function(con, log_table_name, log_data) {
  verify_table(con, log_table_name, log_data, identical_columns = TRUE,
               context_info = "Cannot import data")
}
