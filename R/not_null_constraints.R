NotNullConstraints <- R6::R6Class(
  "NotNullConstraints",

  private = list(
    not_nullable = NULL
  ),

  public = list(
    initialize = function(con) {
      private$not_nullable <- get_not_nullable(con)
    },

    is_nullable = function(table_name, column_name) {
      nrow(private$not_nullable[
        private$not_nullable$table_name == table_name &
        private$not_nullable$column_name == column_name, ]) == 0
    },

    not_nulls = function(table_name) {
      data <- private$not_nullable[
        private$not_nullable$table_name == table_name, "column_name"]
      if (length(data) == 0) {
        data <- NULL
      }
      data
    },

    is_serial = function(table_name, column_name) {
      is_serial <- private$not_nullable[
        private$not_nullable$table_name == table_name &
        private$not_nullable$column_name == column_name, "is_serial"]
      if (length(is_serial) == 0) {
        is_serial <- FALSE
      }
      is_serial
    }
  )
)

get_not_nullable <- function(con) {
  UseMethod("get_not_nullable")
}

get_not_nullable.PqConnection <- function(con) {
  path <- dettl_file("sql", "postgresql", "get_not_null.sql")
  query <- read_lines(path)
  DBI::dbGetQuery(con, query)
}

get_not_nullable.SQLiteConnection <- function(con) {
  tables <- DBI::dbListTables(con)

  constraints <- data_frame(table_name = character(0),
                            column_name = character(0))
  queries <- lapply(tables, function(table) {
    sprintf(
      "SELECT '%s' as table_name,
       pragma.'name' as column_name,
       (pragma.'type' = 'INTEGER' AND pragma.'pk')
              OR pragma.'name' = 'rowid'
              OR pragma.'name' = 'oid'
              OR pragma.'name' = '_rowid_' as is_serial
       FROM pragma_table_info('%s') as pragma
       WHERE pragma.'notnull' = 1",
      table, table)
  })

  constraints <- lapply(queries, function(query) {
    DBI::dbGetQuery(con, query)
  })
  constraints <- do.call(rbind, constraints)
  constraints$is_serial <- as.logical(constraints$is_serial)
  constraints
}
