#' Create an import object using functions defined at specified path
#'
#' @param path Path to directory containing functions for import process.
#'
#' @return A dataImport object.
#' @export
#'
new_import <- function(path) {

  dettl_config <- read_config(path)
  import <- dataImport$new(path,
                           extract = dettl_config$extract$func,
                           extract_test = dettl_config$extract$test,
                           transform = dettl_config$transform$func,
                           transform_test = dettl_config$transform$test,
                           load = dettl_config$load$func,
                           load_test = dettl_config$load$test,
                           test_queries = dettl_config$load$verification_queries
                           )
}

#' Run the import process for a data_import object.
#'
#' This runs the full extract, transform, test and load steps or runs specific
#' stages specified by 'run_stages'.
#'
#' @param import The dataImport object.
#' @param run_stages Which stages should be run in the import process. Runs all
#' if NULL.
#'
#' @return The processed dataImport object.
#' @export
#'
run_import <- function(import, run_stages = NULL) {
  if (is.null(import) || class(import) != "dataImport") {
    stop(
      "Can only run import for non null data import with class 'dataImport'.")
  }
  if (is.null(run_stages) || "extract" %in% run_stages) {
    import$extract()
  }
  if (is.null(run_stages) || "transform" %in% run_stages) {
    import$transform()
  }
  if (is.null(run_stages) || "load" %in% run_stages) {
    import$load()
  }
  import
}
