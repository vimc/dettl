#' Automatic load function for create mode imports.
#'
#' The automatic load function loops over the transformed data and creates
#' a table from each data frame. It uses the default types in the db from the
#' types in the data frame and then appends the rows to the newly created
#' tables.
#'
#' This will now allow appending to an existing table.
#'
#' @param transformed_data The list of transformed data frames to create in the
#' database.
#' @param con Connection to the database to add data to.
#'
#' @keywords internal
dettl_auto_load_create <- function(transformed_data, con) {
  create_and_append <- function(table_name) {
    DBI::dbCreateTable(con, table_name, transformed_data[[table_name]])
    DBI::dbAppendTable(con, table_name, transformed_data[[table_name]])
  }
  lapply(names(transformed_data), create_and_append)
  invisible(TRUE)
}
