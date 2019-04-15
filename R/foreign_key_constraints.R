ForeignKeyConstraints <- R6::R6Class(
  "ForeignKeyConstraints",

  private = list(
    constraints = NULL
  ),

  public = list(
    initialize = function(constraints) {
      is_named_list <- function(x) {
        is.list(x) && length(x) > 0 && !is.null(names(x))
      }

      ## Validate input
      if (!is_named_list(constraints)) {
        stop("Rewrite keys must be a named list with length > 0. Check configuration.")
      }

      lapply(names(constraints), function(constraint) {
        if (is.null(constraints[[constraint]]$primary) ||
            is.null((constraints[[constraint]]$foreign))) {
          stop(sprintf(
            "Rewrite keys must specify a primary key and foreign keys for each table. Check configuration for table %s.",
            constraint))
        }
        if (!is_named_list(constraints[[constraint]]$foreign)) {
          stop(sprintf(
            "Foreign keys must be a named list with length > 0 for each table. Check configuration for table %s.",
            constraint))
        }
        lapply(names(constraints[[constraint]]$foreign), function(child_table) {
          foreign_key_field <- constraints[[constraint]]$foreign[[child_table]]
          if (!is.character(foreign_key_field) || length(foreign_key_field) != 1) {
            stop(sprintf(
              "Foreign keys must be a character. Check configuration for referenced table %s and child table %s.",
              constraint, child_table))
          }
        })
      })
      private$constraints <- constraints
    },

    used_as_foreign_key = function(name) {
      name %in% names(private$constraints)
    },

    get_primary_key = function(name) {
      if (self$used_as_foreign_key(name)) {
        primary_key <- private$constraints[[name]]$primary
      } else {
        stop(sprintf(
          "Tried to get primary key for table %s not in configuration.", name))
      }
      primary_key
    },

    get_foreign_key_usages = function(name) {
      if (self$used_as_foreign_key(name)) {
        foreign_key_constraints <- private$constraints[[name]]$foreign
      } else {
        stop(sprintf(
          "Tried to get foreign key usages for primary key of table %s. Missing from configuration.",
          name
        ))
      }
      foreign_key_constraints
    }
  )
)
