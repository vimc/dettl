#' Create an import object using functions defined at specified path
#'
#' @param path Path to directory containing functions for import process.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the `dettl_config.yml`. If name is left blank this will default
#' to using the first db configured.
#'
#' @return A DataImport object.
#' @export
#'
dettl <- function(path, db_name = NULL) {

  path <- normalizePath(path, mustWork = TRUE)
  dettl_config <- read_config(path)

  if (dettl_config$load$default) {
    load_func <- get_default_load(dettl_config$rewrite_keys)
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
