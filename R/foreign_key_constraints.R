ForeignKeyConstraints <- R6::R6Class(
  "ForeignKeyConstraints",

  private = list(
    constraints = NULL,
    constraint_table = NULL
  ),

  public = list(

    initialize = function(con) {
      private$constraint_table <- get_fk_constraints(con)
      private$constraints <- parse_constraints(private$constraint_table)
    },

    used_as_foreign_key = function(name) {
      !is.null(private$constraints[[name]]$foreign)
    },

    is_serial = function(name, column) {
      column %in% private$constraints[[name]]$serial
    },

    has_serial = function(name) {
      length(private$constraints[[name]]$serial) > 0
    },

    get_referenced_keys = function(name) {
      if (self$used_as_foreign_key(name)) {
        constrained_keys <- names(private$constraints[[name]]$foreign)
      } else {
        stop(sprintf(
          "Tried to get referenced keys for table '%s', table is missing from constraints.", name))
      }
      constrained_keys
    },

    get_foreign_key_usages = function(name, constraint_column) {
      if (self$used_as_foreign_key(name)) {
        foreign_key_constraints <-
          private$constraints[[name]]$foreign[[constraint_column]]
      } else {
        stop(sprintf(
          "Tried to get foreign key usages for referenced table '%s' and column '%s', table and column are missing from constraints.",
          name, constraint_column
        ))
      }
      foreign_key_constraints
    },

    is_serial_constraint = function(name, column, data) {
      ## Is a referenced column in a table actually used as a referenced key
      ## for this set of data we are uploading? If so then it needs to be
      ## stripped before upload
      is_serial <- self$is_serial(name, column)
      foreign_key_usages <- self$get_foreign_key_usages(name, column)
      is_used_as_constraint <- FALSE
      for (usage_name in names(foreign_key_usages)) {
        if (usage_name %in% names(data)) {
          table <- data[[usage_name]]
          if (foreign_key_usages[usage_name] %in% colnames(table)) {
            is_used_as_constraint <- TRUE
          }
        }
      }
      is_serial && is_used_as_constraint
    },

    get_network_table = function(data) {
      ## We are only concerned with the subset of tables we are trying to
      ## upload here. If any other required tables don't exist the import
      ## will fail and postgres error will be returned to user
      rows_to_keep <-
        private$constraint_table$constraint_table %in% names(data) &
        private$constraint_table$referenced_table %in% names(data)
      filtered_tables <- private$constraint_table[
        rows_to_keep,
        c("constraint_table", "constraint_column", "referenced_table")]

      ## We can also ignore links where the constrained column is nullable and
      ## all values are NULL or NA - we rely on db to enforce the column being
      ## nullable and check here if all values in that column are null/NA
      ## we can then remove the row when we go to work out our network of
      ## dependencies.
      ## This allows situations like
      ## Table A with columns id, parent where parent references the id column
      ## of the same table. So we have a cycle here but can still upload
      ## to the db happily if the parent column is NULL or NA
      all_empty <- function(column) {
        all(is.na(column) | is.null(column))
      }
      keep_rows <- vlapply(seq_len(nrow(filtered_tables)), function(row_num) {
        r <- filtered_tables[row_num, ]
        !all_empty(data[[r$constraint_table]][[r$constraint_column]])
      })
      filtered_tables <- filtered_tables[keep_rows, ]

      constraints <- unique(filtered_tables$constraint_table)
      dependencies <- lapply(setNames(constraints, constraints), function(table) {
        unique(filtered_tables[filtered_tables$constraint_table == table,
                            "referenced_table"])
      })

      ## We want to ensure that all the tables we want to upload exist in the
      ## network, so create an entry for missing tables with no dependencies
      missing_tables <- names(data)[!(names(data) %in% names(dependencies))]
      c(dependencies, setNames(rep(NA, length(missing_tables)), missing_tables))
    }
  )
)
