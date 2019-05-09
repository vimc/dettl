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

#' Build the data to be written to the import log
#'
#' @param import_path Path to the import process directory.
#' @param comment An optional comment for this import run.
#'
#' @return The prepared data for the log table. This is the name
#' of the import, the date, comment and git information including
#' user name, user email, current branch and hash of HEAD.
#'
#' @keywords internal
build_log_data <- function(import_path, comment) {
  if (is.null(comment)) {
    comment <- NA_character_
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
              context_info = "Cannot import data",
              solution_text = "Please run dettl::dettl_initialise first.")
}

#' Verify that this is the first time the import has been run.
#'
#' Check whether an import with the same name has been run already. If
#' so then stop with a human understandable message.
#'
#' @param con Connection to the DB to be imported to.
#' @param log_table_name The name of the log table to search for records.
#' @param log_data The data which will be written to the log table should
#' the import run.
#'
#' @return Throws an error if an import with the same name has already been run
#'
#' @keywords internal
verify_first_run <- function(con, log_table_name, log_data) {
  previous_runs <- DBI::dbGetQuery(
    con,
    sprintf("SELECT * FROM %s WHERE name = $1", log_table_name),
    log_data$name)
  if (nrow(previous_runs) > 0) {
    stop(sprintf("Import has previously been run. Previous run log:
  name:           %s
  date:           %s
  comment:        %s
  git user.name:  %s
  git user.email: %s
  git branch:     %s
  git hash:       %s",
                 previous_runs[1, ]$name,
                 parse_sql_date(con, previous_runs[1, ]$date),
                 previous_runs[1, ]$comment,
                 previous_runs[1, ]$git_user,
                 previous_runs[1, ]$git_email,
                 previous_runs[1, ]$git_branch,
                 previous_runs[1, ]$git_hash))
  }
}
