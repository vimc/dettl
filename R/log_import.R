log_import <- function(con, log_table, import_path, comment) {
  if (is.null(comment)) {
    comment <- ""
  }
  import_log <- data.frame(name = basename(import_path),
                           date = Sys.time(),
                           comment = comment,
                           git_user = git_user(import_path),
                           git_email = git_email(import_path),
                           git_branch = git_branch(import_path),
                           git_hash = git_hash(import_path),
                           stringsAsFactors = FALSE)
  query <- DBI::dbAppendTable(con, log_table, import_log)
}
