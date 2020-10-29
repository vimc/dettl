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
  mode <- get_mode(path)
  if ((mode %in% c("create", "append"))) {
    import <- RImport$new(path, db_name)
  } else if (mode == "sql") {
    import <- SqlImport$new(path, db_name)
  } else {
    stop(sprintf(paste0("Can't initialise import for unknown mode got \"%s\",",
                        " mode must be one of \"create\" or \"append\" ",
                        "or \"sql\"."),
                 mode))
  }
  import
}

get_mode <- function(path) {
  config <- read_config(path)
  mode <- config$dettl$mode
  assert_scalar_character(mode, "import mode")
  mode
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
#' @param save Path and name to save data from each stage at, if TRUE then will
#' save to a tempfile.
#' @param dry_run If TRUE then any changes to the database will be rolled back.
#' @param allow_dirty_git If TRUE then skips check that the import is up to date
#' with remote git repo.
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
                      stage = c("extract", "transform"), save = FALSE) {
  assert_character(import)
  import_object <- dettl(import, db_name)
  import_object$run_import(comment, dry_run, allow_dirty_git, stage, save)
}
