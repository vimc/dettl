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
  info <- validate_load(info)
  function_fields <- c(
    "extract",
    "transform",
    "load"
  )
  info <- add_missing_function_fields(info, function_fields)
  required <- c(function_fields, "sources")
  optional <- c("rewrite_keys")
  check_fields(info, filename, required, optional)
  if (info$load$default) {
    info$rewrite_keys <- ForeignKeyConstraints$new(info$rewrite_keys)
  }
  env <- load_sources(info$sources, path)
  info <- read_function_fields(function_fields, info, env)
  info$name <- basename(normalizePath(path))
  info$path <- path
  class(info) <- "dettl_config"
  info
}

validate_load <- function(info) {
  default <- !is.null(info$load$default) && (info$load$default || tolower(info$load$default) == "true")
  if (xor(default, !is.null(info$load$func))) {
    info$load$default <- default
  } else {
    stop(sprintf("Load stage must specify a load function OR use the default load function. Got default %s and NULL func %s.",
                 default, is.null(info$load$func)))
  }
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
  ## Func can be empty for load field when running default load
  if (field_name != "load") {
    if (is.null(info[[field_name]]$func) || is.na(info[[field_name]]$func)) {
      info[[field_name]]$func <- field_name
    }
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
#' @param fields The name of the field.
#' @param config The config being read.
#' @param env Environment containing functions loaded from sources.
#' @keywords internal
#'
read_function_fields <- function(fields, config, env) {
  for (field in fields) {
    if (field != "load" || !config$load$default) {
      assert_func_exists(config[[field]]$func, env)
      config[[field]]$func <- get0(config[[field]]$func,
                                   envir = env,
                                   mode = "function", inherits = FALSE
      )
    }
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
  files <- expand_wildcards(sources, path)
  env <- new.env(parent = .GlobalEnv)
  for (source in files) {
    sys.source(source, envir = env)
  }
  env
}

#' Expand any wilcards in source file paths.
#'
#' Expands any wildcards in the sources, prints a message if file with
#' name cannot be found.
#'
#' @param sources List of configured file paths some of which may contain
#' wildcards. Relative to the root of the import.
#' @param path Normalised path to the import directory.
#'
#' @return List of files to be used as sources
#'
#' @keywords internal
expand_wildcards <- function(sources, path) {
  withr::with_dir(path, {
    sources <- unlist(lapply(sources, expand1), FALSE, FALSE)
    sources <- normalizePath(sources, mustWork = TRUE)
  })
  sources
}

expand1 <- function(source) {
  files <- Sys.glob(source)
  if (length(files) == 0) {
    stop(sprintf("No files found matching file pattern %s", source))
  }
  files
}
