#' Get FK constraints
#'
#' @param con Connection to get constraints for.
#'
#' @return A list representing foreign key constraints
#' @keywords internal
get_fk_constraints <- function(con) {
  dialect <- sql_dialect(con)
  get_fks <- switch(
    dialect,
    "sqlite" = get_sqlite_fk,
    "postgresql" = get_postgres_fk,
    stop(sprintf("Only sqlite and postgresql dialects are supported got %s.",
                  dialect))
  )
  parse_constraints(get_fks(con))
}

#' Get FK constraints for SQLite connection
#'
#' @param con SQLite connection to get constraints for.
#'
#' @return A data frame representing foreign key constraints.
#' @keywords internal
get_sqlite_fk <- function(con) {
  tables <- DBI::dbListTables(con)

  constraints <- data.frame(constraint_table = character(0),
                            constraint_column = character(0),
                            referenced_table = character(0),
                            referenced_column = character(0),
                            stringsAsFactors = FALSE)
  queries <- lapply(tables, function(table) {
    sprintf(
      "SELECT pragma.'table' as referenced_table,
        pragma.'from' as constraint_column,
        pragma.'to' as referenced_column,
        '%s' as constraint_table
        FROM  pragma_foreign_key_list('%s') as pragma",
      table, table)
  })

  constraints <- lapply(queries, function(query) {
    DBI::dbGetQuery(con, query)
  })
  do.call(rbind, constraints)
}

#' Get FK constraints for postgres connection
#'
#' @param con Postgres connection to get constraints for.
#'
#' @return A data frame representing foreign key constraints.
#' @keywords internal
get_postgres_fk <- function(con) {
  path <- dettl_file("sql", "postgresql", "get_fk.sql")
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
  constraints <- list("primary" = referenced_column)
  foreign_keys <- list()
  for (fk_table in data$constraint_table) {
    fk_data <- data[data$constraint_table == fk_table, ]
    foreign_keys[[fk_table]] <- fk_data$constraint_column
  }
  constraints$foreign <- foreign_keys
  constraints
}
