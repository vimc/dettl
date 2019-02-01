#' Get dettl config from path.
#'
#' @param path Path to directory containing path.
#'
#' @return The config as a dettl_config object.
#'
#' @keywords internal
#'
dettl_config <- function(path) {
  path <- dettl_locate_config_dir(path)
  filename <- path_dettl_config_yml(path)
  if (!file.exists(filename)) {
      stop("Did not find file 'dettl_config.yml' at path ", path)
  }
  dettl_config_read_yaml(filename, path)
}

path_dettl_config_yml <- function(root) {
    file.path(root, "dettl_config.yml")
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
  path <- find_file_descend("dettl_config.yml", path)
  if (is.null(path)) {
    stop(sprintf("Reached root from %s without finding 'dettl_config.yml'",
                 path))
  }
  path
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

read_config <- function(path) {
  filename <- file.path(path, "dettl.yml")
  assert_file_exists(path, name = "Report working directory")
  assert_file_exists(filename, name = "Orderly configuration")
  info <- yaml_read(filename)

  ## If certain fields don't exist in the config then add defaults
  required <- c("extract",
                "transform",
                "test",
                "load")
  info <- add_missing_required_fields(info, required, path)
  optional <- c()
  check_fields(info, filename, required, optional)

  info <- read_fields(info, path)
  info$name <- basename(normalizePath(path))
  info$path <- path
  class(info) <- "dettl_config"
  info
}

add_missing_required_fields <- function(info, required, path) {
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

get_default_config <- function(name) {
  cfg <- list()
  cfg$func <- name
  cfg$file <- get_default_file(name)
  cfg
}

get_default_file <- function(field) {
  file.path("R", paste0(field, ".R"))
}

read_fields <- function(fields, path) {
  for (field in names(fields)) {
    tryCatch({assert_file_exists(fields[[field]]$file, workdir = path)},
             error = function(e) {
               e$message <- paste0(e$message, " at path ", path)
               stop(e)
             })
    env <- new.env(parent = .GlobalEnv)
    sys.source(file.path(path, fields[[field]]$file), envir = env)
    assert_func_exists(fields[[field]]$func, env)
    fields[[field]]$func <- get0(fields[[field]]$func, envir = env,
                                 mode = "function", inherits = FALSE)
  }
  fields
}


