#' Write the log data to the database.
#'
#' @param con Connection to the database to be written to.
#' @param log_table The name of the table to write to.
#' @param log_data The log data to be written to the DB.
#'
#' @keywords internal
write_log <- function(con, log_table, log_data) {
  DBI::dbAppendTable(con, log_table, log_data)
}

#' Get the data to be written to the import log
#'
#' @param import_path Path to the import process directory.
#' @param comment An optional comment for this import run.
#'
#' @return The prepared data for the log table. This is the name
#' of the import, the date, comment and git information including
#' user name, user email, current branch and hash of HEAD.
#'
#' @keywords internal
get_log_data <- function(import_path, comment) {
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