#' Create an import object using functions defined at specified path
#'
#' @param path Path to directory containing functions for import process.
#'
#' @return A dataImport object.
#' @export
#'
new_import <- function(path) {
  sources <- dir(paste0(path, "/R"), "\\w+\\.R$", full.names = TRUE)
  lapply(sources, source)
  import <- dataImport$new(path,
                           extract = globalenv()$extract,
                           transform = globalenv()$transform,
                           test = globalenv()$test,
                           load = globalenv()$load,
                           connection = globalenv()$start_connection)
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
