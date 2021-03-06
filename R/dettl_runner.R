#' Create an import object using functions defined at specified path
#'
#' @param path Path to directory containing functions for import.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the \code{dettl_config.yml}. If name is left blank this will
#' default to using the first db configured.
#'
#' @return An Import object.
#' @export
#'
dettl <- function(path, db_name = NULL) {
  language <- get_language(path)
  if (identical(language, "r")) {
    import <- RImport$new(path, db_name)
  } else if (identical(language, "sql")) {
    import <- SqlImport$new(path, db_name)
  } else {
    stop(sprintf(paste0("Can't initialise import for unknown language got ",
                        "\"%s\", language must be one of \"R\" or \"sql\"."),
                 language))
  }
  import
}

get_language <- function(path) {
  config <- read_config_yml(path)
  config$dettl$language
}

#' Run specified stages of an import
#'
#' @param import Path to import directory.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the `dettl_config.yml`. If name is left blank this will default
#' to using the first db configured.
#' @param stage The stage or stages of the import to be run.
#' @param comment Optional comment to be written to db log table when import is
#' run.
#' @param dry_run If TRUE then any changes to the database will be rolled back.
#' @param allow_dirty_git If TRUE then skips check that the import is up to date
#' with remote git repo.
#' @param ... Additional args passed to run_import for a specific import type
#' see \code{RImport$run_import()}
#' @return The import object
#'
#' @export
#'
#' @examples
#' path <- dettl:::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
#' dettl::dettl_run(file.path(path, "person_information/"), "test",
#'   comment = "Example import")
#' dettl::dettl_run(file.path(path, "person_information/"), "test",
#'   comment = "Example import",
#'   save = tempfile())
#' import <- dettl::dettl_run(file.path(path, "person_information/"),
#'   "test", stage = "extract")
#' dettl::dettl_run(file.path(path, "person_information/"), "test",
#'   stage = c("extract", "transform", "load"),
#'   comment = "Example import")
dettl_run <- function(import, db_name = NULL, comment = NULL,
                      dry_run = FALSE, allow_dirty_git = FALSE,
                      stage = c("extract", "transform"), ...) {
  assert_character(import)
  import_object <- dettl(import, db_name)
  import_object$run_import(comment, dry_run, allow_dirty_git, stage, ...)
}
