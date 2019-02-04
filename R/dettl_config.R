#' Get db config from path.
#'
#' This locates config from dettl_config.yml containing database connection
#' configuration.
#'
#' @param path Path to directory containing path.
#'
#' @return The config as a db_config object.
#'
#' @keywords internal
#'
db_config <- function(path) {
  path <- dettl_locate_config_dir(path)
  filename <- path_dettl_config_yml(path)
  if (!file.exists(filename)) {
      stop("Did not find file 'db_config.yml' at path ", path)
  }
  db_config_read_yaml(filename, path)
}

path_dettl_config_yml <- function(root) {
    file.path(root, "db_config.yml")
}

#' Locate the directory containing the config file.
#'
#' @param path Path to directory to start looking in.
#'
#' @return The directory containing the dettl_config yaml file.
#'
#' @keywords internal
#'
dettl_locate_config_dir <- function(path) {
  path <- find_file_descend("db_config.yml", path)
  if (is.null(path)) {
    stop(sprintf("Reached root from %s without finding 'dettl_config.yml'",
                 path))
  }
  path
}

#' Read yaml file representing a db config.
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
db_config_read_yaml <- function(filename, path) {
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

  class(info) <- "db_config"
  info
}


#' Get dettl config from path.
#'
#' Get the config containing information about location of functions containing
#' import code.
#'
#' Sources any references to functions ready for use. Usea a default
#' configuration in the result that any fields are missing. This will use the
#' field name as the expected file name and function anme. Required fields are
#' extract, transform, test_queries, test and load.
#'
#' @param path Path to directory containing config.
#'
#' @return The config as a dettl_config object.
#'
#' @keywords internal
#'
read_config <- function(path) {
  filename <- file.path(path, "dettl.yml")
  assert_file_exists(path, name = "Report working directory")
  assert_file_exists(filename, name = "Orderly configuration")
  info <- yaml_read(filename)

  ## If certain fields don't exist in the config then add defaults
  required <- c("extract",
                "transform",
                "test_queries",
                "test",
                "load")
  info <- add_missing_required_fields(info, required)
  optional <- c()
  check_fields(info, filename, required, optional)

  info <- read_fields(info, path)
  info$name <- basename(normalizePath(path))
  info$path <- path
  class(info) <- "dettl_config"
  info
}


#' Add any missing required fields to the config.
#'
#' This defaults to using the field name for the function and file name equal
#' to the field, and function name. For the 'test' field only need the file.
#'
#' @param info The config loaded from file.
#' @param required Collection of required fields.
#'
#' @keywords internal
#'
add_missing_required_fields <- function(info, required) {
  ## Tidy incomplete fields
  for (field_name in names(info)) {
    if (is.null(info[[field_name]]$func) || is.na(info[[field_name]]$func)) {
      info[[field_name]]$func <- field_name
    }
    if (is.null(info[[field_name]]$file) || is.na(info[[field_name]]$file)) {
      info[[field_name]]$file <- get_default_file(field_name)
    }
  }
  ## Handle missing fields
  missing <- setdiff(required, names(info))
  for (missing_field in missing) {
    info[[missing_field]] <- get_default_config(missing_field)
  }
  info
}

#' Create default config for a field
#'
#' Default config is func: field_name, file: R/field_name.R
#'
#' @param name The name of the field
#' @keywords internal
#'
get_default_config <- function(name) {
  cfg <- list()
  cfg$func <- name
  cfg$file <- get_default_file(name)
  cfg
}

get_default_file <- function(field) {
  file.path("R", paste0(field, ".R"))
}

#' Read file fields of the dettl config yaml file.
#'
#' Reads the config file, and sources any file references.
#'
#' @param fields The name of the field
#' @keywords internal
#'
read_fields <- function(fields, path) {
  for (field in names(fields)) {
    assert_file_exists(fields[[field]]$file, workdir = path)
    if (field != "test") {
      env <- new.env(parent = .GlobalEnv)
      sys.source(file.path(path, fields[[field]]$file), envir = env)
      assert_func_exists(fields[[field]]$func, env)
      fields[[field]]$func <- get0(fields[[field]]$func, envir = env,
                                 mode = "function", inherits = FALSE)
    } else {
      fields[[field]]$func <- NULL
    }
  }
  fields
}


