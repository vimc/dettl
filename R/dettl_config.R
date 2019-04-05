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
  config <- dettl_locate_config(path)
  db_config_read_yaml(config$filename, config$path)
}

#' Locate the directory containing the config file.
#'
#' @param path Path to directory to start looking in.
#'
#' @return The directory containing the dettl_config yaml file.
#'
#' @keywords internal
#'
dettl_locate_config <- function(path) {
  config <- list()
  config$path <- find_file_descend("db_config.yml", path)
  if (is.null(config$path)) {
    stop(sprintf(
      "Reached root from %s without finding 'db_config.yml'",
      path
    ))
  }
  config$filename <- file.path(config$path, "db_config.yml")
  config
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
  check_fields(info, filename, "db", "vault_server")

  check_length(info$db, "gt", 0)
  for (db_cfg in names(info$db)) {
    if (is.null(info$db[[db_cfg]]$driver)) {
      stop(sprintf("No driver specified for DB config %s.", db_cfg))
    }
    info$db[[db_cfg]]$driver <- check_symbol_from_str(
      info$db[[db_cfg]]$driver, sprintf("%s:%s:driver", filename, db_cfg)
    )
  }

  if (!is.null(info$vault_server)) {
    assert_scalar_character(info$vault_server,
                             sprintf("%s:vault_server", filename))
  }

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
  assert_file_exists(filename, name = "Dettl configuration")
  info <- yaml_read(filename)

  ## If certain fields don't exist in the config then add defaults
  function_fields <- c(
    "extract",
    "transform",
    "load"
  )
  info <- add_missing_function_fields(info, function_fields)
  required <- c(function_fields, "sources")
  optional <- c("rewrite_keys")
  check_fields(info, filename, required, optional)

  env <- load_sources(info$sources, path)
  info <- read_function_fields(function_fields, info, env)
  if (!is.null(info$rewrite_keys)) {
    info$rewrite_keys <- ForeignKeyConstraints$new(info$rewrite_keys)
  }
  info$name <- basename(normalizePath(path))
  info$path <- path
  class(info) <- "dettl_config"
  info
}


#' Add any missing required fields to the config using defaults where available.
#'
#' This defaults to using the field name for the function name.
#' For the 'test' field also sets the verification queries function default and
#' the test file default.
#'
#' @param info The config loaded from file.
#' @param fields Collection of fields to set defaults for.
#'
#' @keywords internal
#'
add_missing_function_fields <- function(info, fields) {
  for (field_name in fields) {
    info <- set_missing_values(info, field_name)
  }
  info
}

set_missing_values <- function(info, field_name) {
  if (is.null(info[[field_name]]$func) || is.na(info[[field_name]]$func)) {
    info[[field_name]]$func <- field_name
  }
  missing_verification_queries <- field_name == "load" &&
    (is.null(info[[field_name]]$verification_queries) ||
      is.na(info[[field_name]]$verification_queries))
  if (missing_verification_queries) {
    info[[field_name]]$verification_queries <- "verification_queries"
  }
  missing_postload_test <- field_name == "load" &&
    (is.null(info[[field_name]]$test) || is.na(info[[field_name]]$test))
  if (missing_postload_test) {
    info[[field_name]]$test <- "R/test_load.R"
  }
  info
}

#' Read file fields of the dettl config yaml file.
#'
#' Reads the config file, and sources any file references.
#'
#' @param fields The name of the field
#' @param env Environment containing functions loaded from sources.
#' @keywords internal
#'
read_function_fields <- function(fields, config, env) {
  for (field in fields) {
    assert_func_exists(config[[field]]$func, env)
    config[[field]]$func <- get0(config[[field]]$func,
      envir = env,
      mode = "function", inherits = FALSE
    )
    if (field == "load") {
      assert_func_exists(config[[field]]$verification_queries, env)
      config[[field]]$verification_queries <-
        get0(config[[field]]$verification_queries,
          envir = env,
          mode = "function", inherits = FALSE
        )
    }
  }
  config
}

#' Read file fields of the dettl config yaml file.
#'
#' Reads the config file, and sources any file references.
#'
#' @param sources The sources to be loaded
#' @param path Path to locate the sources at.
#'
#' @return Environment child of global environment which sources have been
#' loaded into.
#'
#' @keywords internal
#'
load_sources <- function(sources, path) {
  env <- new.env(parent = .GlobalEnv)
  for (source in sources) {
    assert_file_exists(source, workdir = path)
    sys.source(file.path(path, source), envir = env)
  }
  env
}
