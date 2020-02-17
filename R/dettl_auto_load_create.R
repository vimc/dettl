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
  for (table_name in names(transformed_data)) {
    d <- transformed_data[[table_name]]
    message(sprintf("Creating table '%s' (%d rows x %d columns)",
                    table_name, nrow(d), ncol(d)))
    DBI::dbCreateTable(con, table_name, d)
    DBI::dbAppendTable(con, table_name, d)
  }
  invisible(TRUE)
}
