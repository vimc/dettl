#' Get default load function.
#'
#' Builds the default load function using foreign key constraints read from
#' the database.
#'
#' The default load function loops over the transformed data and appends each
#' data frame to the matching table in the database. If the appended table
#' contains a primary key then when the data is inserted into the database this
#' returns the value of the primary key for the new rows. Then loop over all
#' tables in which this is used as a foreign key and update the previous values
#' to use the returned actual values for the primary key.
#'
#'
#' @return The default load function.
#'
#' @keywords internal
get_default_load <- function() {
  function(transformed_data, con) {
    rewrite_keys <- ForeignKeyConstraints$new(get_fk_constraints(con))
    for (name in names(transformed_data)) {
      if (rewrite_keys$used_as_foreign_key(name)) {
        primary_key <- rewrite_keys$get_primary_key(name)
        old_key_values <- transformed_data[[name]][, primary_key]
        insert_data <- strip_primary_key_column(transformed_data[[name]],
                                                primary_key)
        ids <- insert_into_returning(con, name, insert_data, key = primary_key)
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
  column_names <- names(data) != primary_key
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
