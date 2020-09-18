#' Create an import object using functions defined at specified path
#'
#' @param path Path to directory containing functions for import.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the \code{dettl_config.yml}. If name is left blank this will
#' default to using the first db configured.
#'
#' @return A DataImport object.
#' @export
#'
dettl <- function(path, db_name = NULL) {
  DataImport$new(path, db_name)
}

#' Run specified stages of an import
#'
#' @param import Either path to the import directory or output from a previous
#' call to this function.
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
#' @return List containing the import object and data from each completed stage.
#' Can call dettl_run on this returned list again to execute subsequent stages.
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
#' dettl::dettl_run(import, stage = "transform")
#' dettl::dettl_run(file.path(path, "person_information/"), "test",
#'   stage = c("extract", "transform", "load"),
#'   comment = "Example import")
dettl_run <- function(import, db_name = NULL, stage = c("extract", "transform"),
                      comment = NULL, save = FALSE,
                      dry_run = FALSE, allow_dirty_git = FALSE) {
  import_object <- get_import_object(import, db_name)
  if ("extract" %in% stage) {
    import_object$extract()
  }
  if ("transform" %in% stage) {
    if (is.null(import_object$get_extracted_data())) {
      import_object$extract()
    }
    import_object$transform()
  }
  if ("load" %in% stage) {
    if (is.null(import_object$get_transformed_data())) {
      stop("Can't run load as transform stage has not been run.")
    }
    import_object$load(comment, dry_run, allow_dirty_git)
  }

  if (!isFALSE(save)) {
    if (isTRUE(save)) {
      save <- tempfile(fileext = ".xlsx")
    }
    dettl_save(import_object, save, stage)
  }

  output <- list(
    import = import_object,
    data = list(
      extract = import_object$get_extracted_data(),
      transform = import_object$get_transformed_data()
    )
  )
  class(output) <- "import"
  output
}

get_import_object <- function(import, db_name) {
  UseMethod("get_import_object")
}

get_import_object.character <- function(import, db_name) {
  dettl(import, db_name)
}

get_import_object.import <- function(import, db_name) {
  import$import
}
