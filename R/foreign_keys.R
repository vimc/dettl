#' Get FK constraints
#'
#' @param con Connection to get constraints for.
#'
#' @return A list representing foreign key constraints
#' @keywords internal
get_fk_constraints <- function(con) {
  dialect <- sql_dialect(con)
  constraints <- switch(
    dialect,
    "sqlite" = get_sqlite_fk(con),
    "postgresql" = get_postgres_fk(con, dialect),
    {
      stop(sprintf("Only sqlite and postgresql dialects are supported got %s.",
                   dialect))
    }
  )
  parse_constraints(constraints)
}

#' Get FK constraints for SQLite connection
#'
#' @param con SQLite connection to get constraints for.
#'
#' @return A data frame representing foreign key constraints.
#' @keywords internal
get_sqlite_fk <- function(con) {
  tables <- DBI::dbGetQuery(con,
    "SELECT tbl_name FROM sqlite_master WHERE type = 'table'")

  constraints <- data.frame(matrix(ncol = 4, nrow = 0),
                            stringsAsFactors = FALSE)
  colnames(constraints) <- c("constraint_table", "constraint_column",
                             "referenced_table", "referenced_column")
  for(table in tables$tbl_name) {
    foreign_keys <- DBI::dbGetQuery(con, sprintf(
      "SELECT pragma.'table' as referenced_table,
      pragma.'from' as constraint_column,
      pragma.'to' as referenced_column,
      '%s' as constraint_table
      FROM  pragma_foreign_key_list('%s') as pragma",
      table, table))
    constraints <- rbind(constraints, foreign_keys)
  }
  constraints
}

#' Get FK constraints for postgres connection
#'
#' @param con Postgres connection to get constraints for.
#' @param dialect Dialect name for locating sql query to get constraints.
#'
#' @return A data frame representing foreign key constraints.
#' @keywords internal
get_postgres_fk <- function(con, dialect) {
  path <- system.file("sql", dialect, "get_fk.sql", package = "dettl")
  query <- read_lines(path)
  DBI::dbGetQuery(con, query)
}

#' Parse foreign key constraint data frame into list representation
#'
#' @param constraitns_table Table of constraints to parse.
#'
#' @return A list representing foreign key constraints.
#' @keywords internal
parse_constraints <- function(constraints_table) {
  constraints <- list()
  for(referenced_table in unique(constraints_table$referenced_table)) {
    data <- constraints_table[constraints_table$referenced_table == referenced_table, ]
    constraints[[referenced_table]] <- handle_single_table(referenced_table, data)
  }
  constraints
}

handle_single_table <- function(table_name, data) {
  referenced_column <- unique(data$referenced_column)
  if (length(referenced_column) != 1) {
    stop(sprintf("Primary key for table %s should always be the same. Got primary keys %s.",
                 table_name, paste(referenced_column, collapse = ", ")))
  }
  constraints <- list("primary" = referenced_column)
  foreign_keys <- list()
  for (fk_table in data$constraint_table) {
    fk_data <- data[data$constraint_table == fk_table, ]
    foreign_keys[[fk_table]] <- fk_data$constraint_column
  }
  constraints$foreign <- foreign_keys
  constraints
}
