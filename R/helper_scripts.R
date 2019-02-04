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

#' Run the import process for a directory.
#'
#' This runs the full extract, transform, test and load steps.
#'
#' @param path Path to the directory.
#'
#' @return The dataImport object.
#' @export
#'
run_import <- function(path) {
  import <- new_import(path)
  import$extract()
  import$transform()
  import$load()
  import
}
