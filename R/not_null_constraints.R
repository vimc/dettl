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
       pragma.'name' as column_name
       FROM pragma_table_info('%s') as pragma
       WHERE pragma.'notnull' = 1",
      table, table)
  })

  constraints <- lapply(queries, function(query) {
    DBI::dbGetQuery(con, query)
  })
  do.call(rbind, constraints)
}

