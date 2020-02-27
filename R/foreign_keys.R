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

  constraints <- data_frame(constraint_table = character(0),
                            constraint_column = character(0),
                            referenced_table = character(0),
                            referenced_column = character(0))
  queries <- lapply(tables, function(table) {
    sprintf(
      "SELECT pragma.'table' as referenced_table,
        pragma.'from' as constraint_column,
        pragma.'to' as referenced_column,
        '%s' as constraint_table
        FROM pragma_foreign_key_list('%s') as pragma",
      table, table)
  })

  constraints <- lapply(queries, function(query) {
    DBI::dbGetQuery(con, query)
  })
  constraints <- do.call(rbind, constraints)

  is_serial <- lapply(seq_len(nrow(constraints)), function(row) {
    is_serial_query <- sprintf(
      "SELECT (type = 'INTEGER' AND pk)
              OR name = 'rowid'
              OR name = 'oid'
              OR name = '_rowid_' as ref_is_serial
      FROM pragma_table_info('%s')
      WHERE name = '%s'",
      constraints[row, "referenced_table"],
      constraints[row, "referenced_column"]
    )
    as.logical(DBI::dbGetQuery(con, is_serial_query))
  })
  constraints$ref_is_serial <- unlist(is_serial)
  constraints
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
  for (referenced_table in unique(constraints_table$referenced_table)) {
    data <- constraints_table[
      constraints_table$referenced_table == referenced_table, ]
    constraints[[referenced_table]]$foreign <-
      handle_single_table_fks(referenced_table, data)
    constraints[[referenced_table]]$serial <-
      handle_single_table_serials(referenced_table, data)
  }
  constraints
}

handle_single_table_fks <- function(table_name, data) {
  referenced_columns <- unique(data$referenced_column)
  foreign <- vector("list", length(referenced_columns))
  names(foreign) <- referenced_columns
  index <- 1
  for (referenced_column in referenced_columns) {
    ref_data <- data[data$referenced_column == referenced_column, ]
    foreign_keys <- list()
    for (fk_table in ref_data$constraint_table) {
      fk_data <- ref_data[ref_data$constraint_table == fk_table, ]
      foreign_keys[[fk_table]] <- fk_data$constraint_column
    }
    foreign[[referenced_column]] <- foreign_keys
    index <- index + 1
  }
  foreign
}

handle_single_table_serials <- function(table_name, data) {
  serials <- unique(data[data$ref_is_serial, "referenced_column"])
  if (length(serials) == 0) {
    serials <- NULL
  }
  serials
}
