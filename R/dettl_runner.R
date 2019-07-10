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

  path <- normalizePath(path, winslash = '/', mustWork = TRUE)
  dettl_config <- read_config(path)

  if (dettl_config$load$automatic) {
    load_func <- dettl_auto_load
  } else {
    load_func <- dettl_config$load$func
  }

  import <- DataImport$new(path,
                           extract = dettl_config$extract$func,
                           extract_test = dettl_config$extract$test,
                           transform = dettl_config$transform$func,
                           transform_test = dettl_config$transform$test,
                           load = load_func,
                           load_test = dettl_config$load$test,
                           test_queries = dettl_config$load$verification_queries,
                           db_name = db_name
                           )
}

#' Run extract stage of an import
#'
#' @param path Path to the import directory.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the `dettl_config.yml`. If name is left blank this will default
#' to using the first db configured.
#'
#' @return The extracted data.
#' @export
#'
#' @examples
#' path <- dettl::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
#' dettl::dettl_run_extract(file.path(path, "person_information/"))
dettl_run_extract <- function(path, db_name = NULL) {
  import <- dettl(path, db_name)
  import$extract()
}

#' Run up to and including the transform stage of an import
#'
#' @param path Path to the import directory.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the `dettl_config.yml`. If name is left blank this will default
#' to using the first db configured.
#'
#' @return The transformed data.
#' @export
#'
#' @examples
#' path <- dettl::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
#' dettl::dettl_run_transform(file.path(path, "person_information/"))
dettl_run_transform <- function(path, db_name = NULL) {
  import <- dettl(path, db_name)
  import$extract()
  import$transform()
}

#' Run up to and including the load stage of an import
#'
#' This will run all stages and make updates to the database unless run in
#' dry_run mode.
#'
#' @param path Path to the import directory.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the `dettl_config.yml`. If name is left blank this will default
#' to using the first db configured.
#' @param comment Optional comment to be written to db log table when import is
#' run.
#' @param dry_run If TRUE then any changes to the database will be rolled back.
#' @param force If TRUE then checks the import is up to date with remote git
#' repo will be skipped.
#'
#' @export
#'
#' @examples
#' path <- dettl::prepare_test_import(
#'   system.file("examples", "person_information", package = "dettl"),
#'   system.file("examples", "dettl_config.yml", package = "dettl")
#' )
#' dettl::dettl_run_load(file.path(path, "person_information/"), "test",
#'   comment = "Example import")
dettl_run_load <- function(path, db_name = NULL, comment = NULL,
                           dry_run = FALSE, force = FALSE) {
  import <- dettl(path, db_name)
  import$extract()
  import$transform()
  import$load(comment, dry_run, force)
}
