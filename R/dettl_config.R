#' Get dettl config from path.
#'
#' @param path Path to directory containing path.
#'
#' @return The config as a dettl_config object.
#'
#' @keywords internal
#'
dettl_config <- function(path) {
  filename <- path_dettl_config_yml(path)
  if (!file.exists(filename)) {
    stop("Did not find file 'dettl_config.yml' at path ", path)
  }
  dettl_config_read_yaml(filename, path)
}

path_dettl_config_yml <- function(root) {
  file.path(root, "dettl_config.yml")
}

#' Read yaml file representing a dettl config.
#'
#' Reads the file and parses the returned data.
#'
#' @param filename Path with filename to config yaml file.
#' @param path Path to directory containing path.
#'
#' @return The config as a dettl_config object.
#'
#' @keywords internal
#'
dettl_config_read_yaml <- function(filename, path) {
  info <- yaml_read(filename)
  check_fields(info, filename, "source", "destination")

  driver_config <- function(name) {
    if (name == "destination" && is.null(info[[name]])) {
      ## Fall back to default destination DB
      info[[name]] <- list(driver = "RSQLite::SQLite",
                           args = list(dbname = "dettl.sqlite"))
    }
    driver <- check_symbol_from_str(info[[name]]$driver,
                                    sprintf("%s:%s:driver", filename, name))
    list(driver = driver, args = info[[name]]$args)
  }

  info$source <- driver_config("source")
  info$destination <- driver_config("destination")
  info$path <- normalizePath(path, mustWork = TRUE)

  class(info) <- "dettl_config"
  info
}
