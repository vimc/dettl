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
#' @param id The field to return, defaults to using "id"
#'
#' @return List of returned ids from inserted values.
#'
#' @keywords internal
insert_into_returning <- function(con, table, d, key = NULL, id = NULL) {
  UseMethod("insert_into_returning")
}

insert_into_returning.SQLiteConnection <- function(con, table, d, key = NULL,
                                               id = NULL) {
  id <- id %||% (if (length(key) == 1L) key else "id")
  stopifnot(length(id) == 1L)
  insert1 <- function(i) {
    x <- as.list(d[i, , drop = FALSE])
    x <- x[!vlapply(x, is.na)]
    sql <- c(sprintf("INSERT INTO %s", table),
             sprintf("  (%s)", paste(names(x), collapse = ", ")),
             "VALUES",
             sprintf("  (%s)", paste0("$", seq_along(x), collapse = ", "))
            )
    sql <- paste(sql, collapse = "\n")
    if (is.null(key)) {
      query <- DBI::dbSendQuery(con, sql, unname(x))
      DBI::dbClearResult(query)
      ## TODO: What happens here if we insert but the table has no rowid?
      rowid_last_entry <- DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
      DBI::dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE rowid = $2", id, table), rowid_last_entry)[[1]]
    } else {
      ## Try and retrieve first:
      sql_get <- c(sprintf("SELECT %s FROM %s WHERE", id, table),
                   paste(sprintf("%s = $%d", key, seq_along(key)),
                         collapse = " AND "))
      ret <- DBI::dbGetQuery(con, paste(sql_get, collapse = "\n"),
                             unname(x[key]))[[id]]
      if (length(ret) == 0L) {
        query <- DBI::dbSendQuery(con, sql, unname(x))
        DBI::dbClearResult(query)
        rowid_last_entry <- DBI::dbGetQuery(con, "SELECT last_insert_rowid()")[1, 1]
        ret <- DBI::dbGetQuery(con, sprintf("SELECT %s FROM %s WHERE rowid = $1", id, table), rowid_last_entry)[[1]]
      }
      ret
    }
  }

  if (!is.data.frame(d)) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
  }
  lapply(seq_len(nrow(d)), insert1)
}

insert_into_returning.PqConnection <- function(con, table, d, key = NULL, id = NULL) {
  id <- id %||% (if (length(key) == 1L) key else "id")
  stopifnot(length(id) == 1L)
  insert1 <- function(i) {
    x <- as.list(d[i, , drop = FALSE])
    x <- x[!vlapply(x, is.na)]
    sql <- c(sprintf("INSERT INTO %s", table),
             sprintf("  (%s)", paste(names(x), collapse = ", ")),
             "VALUES",
             sprintf("  (%s)", paste0("$", seq_along(x), collapse = ", ")),
             sprintf("RETURNING %s", id))
    sql <- paste(sql, collapse = "\n")
    if (is.null(key)) {
      ret <- DBI::dbGetQuery(con, sql, unname(x))[[id]]
    } else {
      ## Try and retrieve first:
      sql_get <- c(sprintf("SELECT %s FROM %s WHERE", id, table),
                   paste(sprintf("%s = $%d", key, seq_along(key)),
                         collapse = " AND "))
      ret <- DBI::dbGetQuery(con, paste(sql_get, collapse = "\n"),
                             unname(x[key]))[[id]]
      if (length(ret) == 0L) {
        ret <-  DBI::dbGetQuery(con, sql, unname(x))[[id]]
      }
    }
    if (bit64::is.integer64(ret)) {
      ret <- as.integer(ret)
    }
    ret
  }

  if (!is.data.frame(d)) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
  }
  lapply(seq_len(nrow(d)), insert1)
}
