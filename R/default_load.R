#' Get default load function.
#'
#' Builds the default load function using foreign key constraints read from
#' the database.
#'
#' The default load function loops over the transformed data and appends each
#' data frame to the matching table in the database. If the appended table
#' contains a key referenced by one of the foreign key constraints then when
#' the data is inserted into the database this returns the value of the key for
#' the new rows. Then loop over all tables in which this is used as a foreign
#' key and update the previous values to use the returned actual values for the
#' referenced key.
#'
#' @return The default load function.
#'
#' @keywords internal
get_default_load <- function() {
  function(transformed_data, con) {
    rewrite_keys <- ForeignKeyConstraints$new(con)
    for (name in names(transformed_data)) {
      if (rewrite_keys$used_as_foreign_key(name)) {
        referenced_keys <- rewrite_keys$get_referenced_keys(name)
        insert_data <- strip_referenced_key_columns(transformed_data[[name]],
                                                    referenced_keys)
        return <- insert_into_returning(con, name, insert_data,
                                        return = referenced_keys)
        for (col in names(return)) {
          table_key_pair <- rewrite_keys$get_foreign_key_usages(name, col)
          old_key_values <- transformed_data[[name]][, col]
          transformed_data <- update_child_tables(
            transformed_data, table_key_pair, old_key_values, return[[col]])
        }
      } else {
        DBI::dbWriteTable(con, name, transformed_data[[name]], append = TRUE)
      }
    }
  }
}

strip_referenced_key_columns <- function(data, referenced_keys) {
  columns <- which(!(names(data) %in% referenced_keys))
  data[, columns]
}

#' Update child tables using inserted foreign keys
#'
#' @param tables List of tables to be updated.
#' @param table_key_pair List of child tables and the foreign key field.
#' @param old_key_values Old values of referenced key, how foreign key is
#' currently identified in child table.
#' @param new_key_values New values of referenced key, what foreign key should
#' be updated to.
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
