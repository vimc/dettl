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
      name %in% names(private$constraints)
    },

    get_primary_key = function(name) {
      if (self$used_as_foreign_key(name)) {
        primary_key <- private$constraints[[name]]$primary
      } else {
        stop(sprintf(
          "Tried to get primary key for table '%s', table is missing from constraints.", name))
      }
      primary_key
    },

    get_foreign_key_usages = function(name) {
      if (self$used_as_foreign_key(name)) {
        foreign_key_constraints <- private$constraints[[name]]$foreign
      } else {
        stop(sprintf(
          "Tried to get foreign key usages for primary key of table '%s', table is missing from constraints.",
          name
        ))
      }
      foreign_key_constraints
    }
  )
)
