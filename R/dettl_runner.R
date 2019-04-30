#' Create an import object using functions defined at specified path
#'
#' @param path Path to directory containing functions for import process.
#' @param db_name The name of the db to connect to. Connection info must be
#' configured via the `db_config.yml`. If name is left blank this will default
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

#' Run the import process for a data_import object.
#'
#' This runs the full extract, transform, test and load steps or runs specific
#' stages specified by 'run_stages'.
#'
#' @param import The DataImport object.
#' @param run_stages Which stages should be run in the import process. Runs all
#' if NULL.
#' @param dry_run If TRUE then any database changes are rolled back when the
#' import completes. i.e. the load stage can be run and tests executed but the
#' db will be rolled back.
#'
#' @return The processed DataImport object.
#' @export
#'
run_import <- function(import, run_stages = NULL, dry_run = FALSE) {
  if (is.null(import) || !inherits(import, "DataImport")) {
    stop(
      "Can only run import for non null data import with class 'DataImport'.")
  }
  if (!git_is_clean(import$path) && !dry_run) {
    stop("Can't run import as repository has unstaged changes. Update git or run in dry-run mode.")
  }
  message(sprintf("Running import %s", import$path))
  if (is.null(run_stages) || "extract" %in% run_stages) {
    import$extract()
  }
  if (is.null(run_stages) || "transform" %in% run_stages) {
    import$transform()
  }
  if (is.null(run_stages) || "load" %in% run_stages) {
    import$load(dry_run)
  }
  invisible(import)
}
