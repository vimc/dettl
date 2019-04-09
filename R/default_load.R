## impl and test
get_default_load <- function(rewrite_keys) {
  function(transformed_data, con) {
    for (name in names(transformed_data)) {
      if (rewrite_keys$used_as_foreign_key(name)) {
        primary_key <- rewrite_keys$get_primary_key(name)
        old_key_values <- transformed_data[[name]][, primary_key]
        insert_data <- strip_primary_key_column(transformed_data[[name]],
                                                primary_key)
        ids <- insert_values_into(con, name, insert_data, key = primary_key)
        table_key_pair <- rewrite_keys$get_foreign_key_usages(name)
        transformed_data <- update_child_tables(
          transformed_data, table_key_pair, old_key_values, ids)
      } else {
        DBI::dbWriteTable(con, name, transformed_data[[name]], append = TRUE)
      }
    }
  }
}

strip_primary_key_column <- function(data, primary_key) {
  column_names <- !(names(data) == primary_key)
  data[, column_names]
}

#' Update child tables using inserted foreign keys
#'
#' @param tables List of tables to be updated.
#' @param table_key_pair List of child tables and the foreign key field.
#' @param old_key_values Old values of primary key, how foreign key is currently
#' identified in child table.
#' @param new_key_values New values of primary key, what foreign key should be
#' updated to.
#'
#' @return The updated tables
#'
#' @keywords internal
update_child_tables <- function(tables, table_key_pair, old_key_values, new_key_values) {
  update1 <- function(table_name) {
    table <- tables[[table_name]]
    if (!is.null(table_key_pair[[table_name]])) {
      foreign_key <- table_key_pair[[table_name]]
      table[, foreign_key] <-
        unlist(map_values(table[, foreign_key], old_key_values, new_key_values),
               FALSE, FALSE)
    }
    table
  }
  list_names <- names(tables)
  tables <- lapply(list_names, update1)
  names(tables) <- list_names
  tables
}

## Update any old values within data to new ones
## Values must be unique in old and new
map_values <- function(data, old, new) {
  stopifnot(length(unique(old)) == length(old) &&
            length(unique(new)) == length(new) &&
            length(new) == length(old))
  indices <- vnapply(data, function(x) {
    which(old == x)
  })
  new[indices]
}

#' Insert values into table returning the primary key
#'
#' Checks whether the table already has values matching
#'
#' @param con Connection to db to insert values into.
#' @param table Name of table to insert values into.
#' @param d Data frame of data to be inserted into db.
#' @param key Primary key or if multiple then as composite primary key to be
#' checked before data is inserted into db.
#' @param id The field to return defaults to using "id"
#'
#' @return List of returned ids from inserted values.
#'
#' @keywords internal
insert_values_into <- function(con, table, d, key = NULL, id = NULL) {
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
      DBI::dbGetQuery(con, sql, unname(x))[[id]]
    } else {
      ## Try and retrieve first:
      sql_get <- c(sprintf("SELECT %s FROM %s WHERE", id, table),
                   paste(sprintf("%s = $%d", key, seq_along(key)),
                         collapse = " AND "))
      ret <- DBI::dbGetQuery(con, paste(sql_get, collapse = "\n"),
                             unname(x[key]))[[id]]
      if (length(ret) == 0L) {
        ret <- insert_data(con, sql, x, id)
      }
      ret
    }
  }

  if (!is.data.frame(d)) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
  }
  lapply(seq_len(nrow(d)), insert1)
}

insert_data <- function(con, sql, x, id) {
  DBI::dbGetQuery(con, sql, unname(x))[[id]]
}
