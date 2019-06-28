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
    if (length((names(x))) > 0) {
      ret <- sqlite_insert_row(con, table, x, key, return)
    } else {
      ret <- sqlite_insert_empty_row(con, table, return)
    }
    ret
  }

  rows <- lapply(seq_len(nrow(d)), insert1)
  do.call(rbind.data.frame, rows)
}

sqlite_insert_row <- function(con, table, x, key, return) {
  sql <- c(sprintf("INSERT INTO %s", table),
           sprintf("  (%s)", paste(names(x), collapse = ", ")),
           "VALUES",
           sprintf("  (%s)", paste0("$", seq_along(x), collapse = ", "))
  )
  sql <- paste(sql, collapse = "\n")
  if (is.null(key)) {
    ret <- sqlite_execute_query(con, sql, table, return, x)
  } else {
    ## Try and retrieve first:
    sql_get <- c(sprintf("SELECT %s FROM %s WHERE",
                         paste(return, collapse = ", "), table),
                 paste(sprintf("%s = $%d", key, seq_along(key)),
                       collapse = " AND "))
    ret <- DBI::dbGetQuery(con, paste(sql_get, collapse = "\n"), unname(x[key]))
    if (nrow(ret) == 0L) {
      ret <- sqlite_execute_query(con, sql, table, return, x)
    }
  }
  ret
}

#' Insert an empty row and returned desired columns.
#'
#' @param con Connection to the DB.
#' @param table The table to add row to.
#' @param return Vector of column names to be returned.
#'
#' @return Data frame with the newly inserted values for the specified column.
#'
#' @keywords internal
sqlite_insert_empty_row <- function(con, table, return) {
  sql <- sprintf("INSERT INTO %s DEFAULT VALUES", table)
  sqlite_execute_query(con, sql, table, return)
}

#' Execute sql insert query returning added columns desired columns.
#'
#' @param con Connection to the DB.
#' @param sql The sql insert query.
#' @param table The table to insert data into.
#' @param x Params for the sql query.
#' @param return Vector of column names to be returned.
#'
#' @return Data frame with the newly inserted values for the specified column.
#'
#' @keywords internal
sqlite_execute_query <- function(con, sql, table, return, x = NULL) {
  ## TODO: What happens here if we insert but the table has no rowid?
  ## see https://www.sqlite.org/withoutrowid.html for ref
  if (is.null(x)) {
    DBI::dbExecute(con, sql)
  } else {
    DBI::dbExecute(con, sql, unname(x))
  }
  rowid_last_entry <- DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
  get_columns_sql <- sprintf("SELECT %s FROM %s WHERE rowid = $1",
                             paste(return, collapse = ", "),
                             table)
  DBI::dbGetQuery(con, get_columns_sql, rowid_last_entry)
}

insert_into_returning.PqConnection <- function(con, table, d, key = NULL,
                                               return = NULL) {
  return <- return %||% (if (length(key) >= 1L) key else "id")
  insert1 <- function(i) {
    x <- as.list(d[i, , drop = FALSE])
    x <- x[!vlapply(x, is.na)]
    if (length((names(x))) > 0) {
      ret <- postgres_insert(con, table, x, key, return)
    } else {
      ret <- postgres_insert_empty_row(con, table, return)
    }
    ret <- convert_bit64_columns(ret)
  }

  rows <- lapply(seq_len(nrow(d)), insert1)
  do.call(rbind.data.frame, rows)
}

postgres_insert <- function(con, table, x, key, return) {
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
  ret
}


#' Insert an empty row and returned desired columns.
#'
#' @param con Connection to the DB.
#' @param table The table to add row to.
#' @param return Vector of column names to be returned.
#'
#' @return Data frame with the newly inserted values for the specified column.
#'
#' @keywords internal
postgres_insert_empty_row <- function(con, table, return) {
  ## In this case we want to insert row and only populate autoincrement rows
  ## Insert an empty row
  sql <- sprintf("INSERT INTO %s DEFAULT VALUES RETURNING %s",
                 table,
                 paste(return, collapse = ", "))
  DBI::dbGetQuery(con, sql)
}


#' Convert any bit64 type columns to integers.
#'
#' Postgres returns bit64 types for integer fields.
#'
#' @param data The data frame with columns to be converted.
#'
#' @return Data frame with with any bit64 columns converted to integers.
#'
#' @keywords internal
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
