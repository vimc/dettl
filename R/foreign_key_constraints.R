ForeignKeyConstraints <- R6::R6Class(
  "ForeignKeyConstraints",

  private = list(
    constraints = NULL
  ),

  public = list(
    initialize = function(con) {
      private$constraints <- get_fk_constraints(con)
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
    }
  )
)
