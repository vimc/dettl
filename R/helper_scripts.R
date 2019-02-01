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
                           transform = dettl_config$transform$func,
                           test = dettl_config$test$func,
                           load = dettl_config$load$func)
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
  import$test()
  import$load()
  import
}
