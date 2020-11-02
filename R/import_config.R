#' Get dettl config from path.
#'
#' Get the config containing information about location of functions containing
#' import code.
#'
#' Sources any references to functions ready for use. Use a default
#' configuration in the result that any fields are missing. This will use the
#' field name as the expected file name and function name. Required fields are
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
  assert_file_exists(path, check_case = FALSE,
                     name = "Import working directory")
  assert_file_exists(basename(filename), workdir = path,
                     name = "Dettl configuration")
  info <- yaml_read(filename)

  ## If certain fields don't exist in the config then add defaults
  info <- validate_load(info)
  function_fields <- list(
    extract = list(
      list(
        func = "func",
        must_exist = TRUE
      )
    ),
    transform = list(
      list(
        func = "func",
        must_exist = TRUE
      )
    ),
    load = list(
      list(
        func = "func",
        must_exist = !info$load$automatic
      ),
      list(
        func = "verification_queries",
        must_exist = TRUE
      ),
      list(
        func = "pre",
        must_exist = FALSE
      ),
      list(
        func = "post",
        must_exist = FALSE
      )
    )
  )
  info <- add_missing_function_fields(info, names(function_fields))
  required <- c(names(function_fields), "sources")
  check_fields(info, filename, required, "dettl")
  env <- load_sources(info$sources, path)
  info <- read_function_fields(function_fields, info, env)
  info$name <- basename(normalizePath(path, winslash = "/"))
  info$path <- path
  info$dettl$mode <- check_valid_mode(info$dettl$mode)
  info$dettl$transaction <-
    is.logical(info$dettl$transaction) %?% info$dettl$transaction %:% TRUE
  class(info) <- "dettl_import_config"
  info
}

validate_load <- function(info) {
  auto <- !is.null(info$load$automatic) && info$load$automatic
  if (xor(auto, !is.null(info$load$func))) {
    info$load$automatic <- auto
  } else {
    stop(sprintf(
      "Load stage must specify a load function OR use the automatic load function. Got automatic %s and NULL func %s.",
      auto, is.null(info$load$func)))
  }
  if (!auto && (!is.null(info$load$pre) || !is.null(info$load$post))) {
    stop(sprintf("Pre or post load are configured but using a custom load step. Pre and post load can only be used with automatic load."))
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
  ## Func can be empty for load field when using automatic load
  if (field_name != "load") {
    if (is.null(info[[field_name]]$func)) {
      info[[field_name]]$func <- field_name
    }
  }
  missing_verification_queries <- field_name == "load" &&
    is.null(info[[field_name]]$verification_queries)
  if (missing_verification_queries) {
    info[[field_name]]$verification_queries <- "verification_queries"
  }
  missing_postload_test <- field_name == "load" &&
    is.null(info[[field_name]]$test)
  if (missing_postload_test) {
    info[[field_name]]$test <- "R/test_load.R"
  }
  info
}

#' Read function fields of the dettl config yaml file.
#'
#' Reads the config file, and sources any file references.
#'
#' @param fields List of fields with functions and whether they have to exist.
#' @param config The config being read.
#' @param env Environment containing functions loaded from sources.
#' @keywords internal
#'
read_function_fields <- function(fields, config, env) {
  for (field in names(fields)) {
    for (property in fields[[field]]) {
      func_name <- config[[field]][[property$func]]
      if (is.null(func_name) && property$must_exist) {
        stop(sprintf("Can't find required function %s for field %s",
                     property$func, field))
      }
      if (!is.null(func_name)) {
        assert_func_exists(func_name, env)
        config[[field]][[property$func]] <- get0(func_name,
                                                 envir = env,
                                                 mode = "function",
                                                 inherits = FALSE)
      }
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

#' Expand any wildcards in source file paths.
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
    sources <- normalizePath(sources, winslash = "/", mustWork = TRUE)
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
