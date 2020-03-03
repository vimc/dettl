#' Get non-empty columns
#'
#' These are the columns which contain any data and so we cannot ignore fk
#' constraints on these columns
#'
#' @param data Lnames list of data frames representing db tables
#'
#' @return Pairs of table to non-empty columns as a list
#'
#' @keywords internal
get_non_empty_columns <- function(data) {
  has_data <- function(d) {
    names(d)[!vlapply(d, function(x) all(is.na(x)))]
  }
  lapply(data, has_data)
}

#' Construct list of network dependencies for use in building upload order
#'
#' @param constraints Table of constraints from db schema the important
#' columns are
#' constraint_table, constraints_column, referenced_table, referenced_column
#' where constraint_* is the table and column the constraint is on and the
#' referenced_* is the table and column the constraint references so e.g.
#' for postgres a constraint like
#' "constr_name" FOREIGN KEY (model_version) REFERENCES model_version(id)
#' on table burden_estimate_set would have a row like
#' constraint_table, constraints_column, referenced_table, referenced_column
#' burden_estimate_set, model_version, model_version, id
#' @param tables Names of tables being uploaded
#' @param non_empty_columns Names list of columns in tables which are non-empty
#' and so we must construct the network of fks respecting these present columns.
#' Other constraints can be ignored for working out the order.
#'
#' @return A list of table to dependencies
#' A | B, C
#' B | C
#' C | NA
#' Meaning B, C must exist before A can be added and C must exist before B
#' can be added and C has no dependencies
#'
#' @keywords internal
get_network_dependencies <- function(constraints, tables, non_empty_columns) {
  ## We are only concerned with the subset of tables we are trying to
  ## upload here. If any other required tables don't exist the import
  ## will fail and postgres error will be returned to user
  rows_to_keep <-
    constraints$constraint_table %in% tables &
    constraints$referenced_table %in% tables
  filtered_tables <- constraints[
    rows_to_keep,
    c("constraint_table", "constraint_column", "referenced_table")]

  ## If the data in the columns is all NA or the columns are missing from
  ## the data we are trying to upload then we can ignore any fk constraints
  ## on these meaning that it may enable us to upload data when there is a
  ## cyclic dependency. To work out the missing columns would require knowledge
  ## of the schema so instead we work here with the non-NA and non-missing
  ## columns i.e. those columns that we have to be wary of the fk constraints
  ## for.
  ## This allows situations like
  ## Table A with columns id, parent where parent references the id column
  ## of the same table. So we have a cycle here but can still upload
  ## to the db happily if the parent column is NA or missing
  ## Note that this puts us at risk of violating not-null constraints but we
  ## rely on postgres to throw an error on upload if this is violated
  keep_rows <- function(table, column) {
    column %in% non_empty_columns[[table]]
  }
  rows_to_keep <- Map(keep_rows,
                      filtered_tables$constraint_table,
                      filtered_tables$constraint_column)
  filtered_tables <- filtered_tables[unlist(rows_to_keep), ]

  constraints <- unique(filtered_tables$constraint_table)
  dependencies <- lapply(setNames(constraints, constraints), function(table) {
    unique(filtered_tables[filtered_tables$constraint_table == table,
                           "referenced_table"])
  })

  ## We want to ensure that all the tables we want to upload exist in the
  ## network, so create an entry for missing tables with no dependencies
  missing_tables <- tables[!(tables %in% names(dependencies))]
  c(dependencies, setNames(rep(NA, length(missing_tables)), missing_tables))
}

## This algorithm comes from here:
## http://blog.jupo.org/2012/04/06/topological-sorting-acyclic-directed-graphs/
## and assumes that the graph is expressed as a *named* list.  The
## daughters of an element are its dependencies.
topological_order <- function(graph) {
  m <- matrix(FALSE, length(graph), length(graph))
  for (i in seq_along(graph)) {
    m[, i] <- unname(names(graph) %in% graph[[i]])
  }
  pending <- rep(TRUE, length(graph))
  graph_sorted <- integer(0)
  while (any(pending)) {
    i <- which(pending)[colSums(m[, pending, drop = FALSE]) == 0]
    if (length(i) > 0L) {
      graph_sorted <- c(graph_sorted, i)
      pending[i] <- FALSE
      m[i, ] <- FALSE
    } else {
      f <- function(i) {
        ## Note that this is not going to give the right answer here
        ## but it might still be useful (dim_x -> dim(x), initial_x ->
        ## initial(x) etc.)  Could swap these around with
        ## RESERVED_PREFIX perhaps.
        sprintf("  %s: depends on %s",
                names(graph)[[i]], paste(err[m[pending, i]], collapse = ", "))
      }
      err <- names(graph)[pending]
      detail <- paste(vcapply(which(pending), f), collapse = "\n")
      stop(sprintf(
        "A cyclic dependency detected for %s:\n%s\nPlease write a custom load",
        paste(names(graph)[pending], collapse = ", "),
        detail), call. = FALSE)
    }
  }
  names(graph)[graph_sorted]
}
