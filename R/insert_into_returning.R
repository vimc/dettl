#' Insert values into table returning the primary key
#'
#' Checks whether the table already has values matching
#'
#' @param con Connection to db to insert values into.
#' @param table Name of table to insert values into.
#' @param d Data frame of data to be inserted into db.
#' @param key Keys to check for already existing in the database. If trying to
#' add a new row with unique key constraints use this to check for existing
#' entries with same information.
#' @param return The field to return, defaults to using "id"
#'
#' @return List of returned ids from inserted values.
#'
#' @keywords internal
insert_into_returning <- function(con, table, d, key = NULL, return = NULL) {
  UseMethod("insert_into_returning")
}

insert_into_returning.SQLiteConnection <- function(con, table, d, key = NULL,
                                               return = NULL) {
  return <- return %||% (if (length(key) >= 1L) key else "id")
  insert1 <- function(i) {
    x <- as.list(d[i, , drop = FALSE])
    x <- x[!vlapply(x, is.na)]
    if (is.null(key)) {
      sqlite_insert(con, table, x, return)
    } else {
      ## Try and retrieve first:
      sql_get <- c(sprintf("SELECT %s FROM %s WHERE",
                           paste(return, collapse = ", "), table),
                   paste(sprintf("%s = $%d", key, seq_along(key)),
                         collapse = " AND "))
      ret <- DBI::dbGetQuery(con, paste(sql_get, collapse = "\n"),
                             unname(x[key]))
      # return_cols <- which(colnames(ret) %in% return)
      # ret <- ret[, return_cols]
      if (nrow(ret) == 0L) {
        ret <- sqlite_insert(con, table, x, return)
      }
      ret
    }
  }

  if (!is.data.frame(d)) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
  }
  rows <- lapply(seq_len(nrow(d)), insert1)
  do.call(rbind.data.frame, rows)
}

#' Run the sql insert query returning desired columns.
#'
#' @param con Connection to the DB.
#' @param table The table to add data to
#' @param x The row of new data to be added
#' @param return Vector of column names to be retuend
#'
#' @return Data frame with the newly inserted values for the specified column
#'
#' @keywords internal
sqlite_insert <- function(con, table, x, return) {
  sql <- c(sprintf("INSERT INTO %s", table),
           sprintf("  (%s)", paste(names(x), collapse = ", ")),
           "VALUES",
           sprintf("  (%s)", paste0("$", seq_along(x), collapse = ", "))
  )
  sql <- paste(sql, collapse = "\n")
  DBI::dbExecute(con, sql, unname(x))
  ## TODO: What happens here if we insert but the table has no rowid?
  ## see https://www.sqlite.org/withoutrowid.html for ref
  rowid_last_entry <- DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
  get_columns_sql <- sprintf("SELECT %s FROM %s WHERE rowid = $1",
                             paste(return, collapse = ", "),
                             table)
  DBI::dbGetQuery(con, get_columns_sql, rowid_last_entry)
}

insert_into_returning.PqConnection <- function(con, table, d, key = NULL, return = NULL) {
  return <- return %||% (if (length(key) >= 1L) key else "id")
  insert1 <- function(i) {
    x <- as.list(d[i, , drop = FALSE])
    x <- x[!vlapply(x, is.na)]
    sql <- c(sprintf("INSERT INTO %s", table),
             sprintf("  (%s)", paste(names(x), collapse = ", ")),
             "VALUES",
             sprintf("  (%s)", paste0("$", seq_along(x), collapse = ", ")),
             sprintf("RETURNING %s", paste(return, collapse = ", ")))
    sql <- paste(sql, collapse = "\n")
    if (is.null(key)) {
      ret <- DBI::dbGetQuery(con, sql, unname(x))
    } else {
      ## Try and retrieve first:
      sql_get <- c(sprintf("SELECT %s FROM %s WHERE",
                           paste(return, collapse = ", "), table),
                   paste(sprintf("%s = $%d", key, seq_along(key)),
                         collapse = " AND "))
      ret <- DBI::dbGetQuery(con, paste(sql_get, collapse = "\n"),
                             unname(x[key]))
      if (nrow(ret) == 0L) {
        ret <-  DBI::dbGetQuery(con, sql, unname(x))
      }
    }
    ret <- convert_bit64_columns(ret)
  }

  if (!is.data.frame(d)) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
  }
  rows <- lapply(seq_len(nrow(d)), insert1)
  do.call(rbind.data.frame, rows)
}

convert_bit64_columns <- function(data) {
  convert <- function(col) {
    if (bit64::is.integer64(col)) {
      as.integer(col)
    } else {
      col
    }
  }
  data[] <- lapply(data, convert)
  data
}
