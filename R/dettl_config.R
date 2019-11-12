#' Get db config from path.
#'
#' This locates config from dettl_config.yml containing database connection
#' configuration.
#'
#' @param path Path to directory containing path.
#'
#' @return The config as a dettl_config object.
#'
#' @keywords internal
#'
dettl_config <- function(path) {
  config <- dettl_locate_config(path)
  dettl_config_read_yaml(config$filename, config$path)
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
  config$path <- find_file_descend("dettl_config.yml", path)
  if (is.null(config$path)) {
    stop(sprintf(
      "Reached root from %s without finding 'dettl_config.yml'",
      path
    ))
  }
  config$filename <- file.path(config$path, "dettl_config.yml")
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
dettl_config_read_yaml <- function(filename, path) {
  info <- yaml_read(filename)
  check_fields(info, filename, "db", "vault_server")

  check_length(info$db, "gt", 0)
  for (db_cfg in names(info$db)) {
    check_fields(info$db[[db_cfg]], filename,
                 c("driver", "log_table", "args"),
                 c("confirm", "require_branch"))
    if (is.null(info$db[[db_cfg]]$driver)) {
      stop(sprintf("No driver specified for DB config %s.", db_cfg))
    }
    info$db[[db_cfg]]$driver <- check_symbol_from_str(
      info$db[[db_cfg]]$driver, sprintf("%s:%s:driver", filename, db_cfg)
    )
    assert_db_name(info$db[[db_cfg]]$log_table,
                   sprintf("%s:%s:log_table", filename, db_cfg))
    if (is.null(info$db[[db_cfg]]$confirm)) {
      info$db[[db_cfg]]$confirm <- FALSE
    }
    if (!is.null(info$db[[db_cfg]]$require_branch)) {
      assert_scalar_character(
        info$db[[db_cfg]]$require_branch,
        sprintf("%s:%s:require_branch", filename, db_cfg))
    }
  }

  if (!is.null(info$vault_server)) {
    assert_scalar_character(info$vault_server,
                             sprintf("%s:vault_server", filename))
  }

  info$path <- normalizePath(path, winslash = '/', mustWork = TRUE)
  class(info) <- "dettl_config"
  info
}

