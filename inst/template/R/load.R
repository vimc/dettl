#' Load transformed data into the database.
#'
#' This step is responsible for running the database update queries to load
#' the transformed data into the database.
#'
#' @param transformed_data The transformed data from the transform stage. In a
#' form which adheres to the DB schema.
#' @param con Active connection to the DB which the data will be loaded to.
#'
#' @keywords internal
load <- function(transformed_data, con) {
  ## Skeleton implementation - overwrite or modify as necessary.
  for (name in names(transformed_data)) {
    DBI::dbWriteTable(con, name, transformed_data[[name]], append = TRUE)
  }
}
