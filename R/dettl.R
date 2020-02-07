##' Manage data import.
##'
##' This object should not be initialised directly. Use \code{\link{dettl}} to
##' create the object.
##'
##' Import can be run by working with import object returned by
##' \code{\link{dettl}} or by running top-level functions. Run the import by
##' working with this object if you want to step through the import process
##' stage by stage and inspect the data after each stage.
##'
##' @template DataImport
##'
##' @title Data Import
##' @name DataImport
##'
##' @examples
##' path <- dettl:::prepare_test_import(
##'   system.file("examples", "person_information", package = "dettl"),
##'   system.file("examples", "dettl_config.yml", package = "dettl"))
##' import_path <- file.path(path, "person_information")
##'
##' import <- dettl::dettl(import_path, db_name = "test")
##' import$extract()
##' import$transform()
##' import$load()
##'
NULL

DataImport <- R6::R6Class(
  "DataImport",
  cloneable = FALSE,

  private = list(
    con = NULL,
    extract_ = NULL,
    transform_ = NULL,
    load_ = NULL,
    extract_test_ = NULL,
    transform_test_ = NULL,
    load_test_ = NULL,
    test_queries = NULL,
    extracted_data = NULL,
    transformed_data = NULL,
    log_table = NULL,
    confirm = NULL,
    require_branch = NULL,
    db_name = NULL
  ),

  public = list(
    path = NULL,
    initialize = function(path, db_name) {
      self$path <- normalizePath(path, winslash = "/", mustWork = TRUE)
      private$db_name <- db_name
      lockBinding("path", self)
      self$reload()
    },

    reload = function() {
      dettl_config <- read_config(self$path)
      if (dettl_config$load$automatic) {
        load_func <- dettl_auto_load
      } else {
        load_func <- dettl_config$load$func
      }
      cfg <- dettl_config(self$path)

      db_name <- private$db_name %||% get_default_type(cfg)
      private$con <- db_connect(db_name, self$path)
      private$extract_ <- dettl_config$extract$func
      private$extract_test_ <- dettl_config$extract$test
      private$transform_ <- dettl_config$transform$func
      private$transform_test_ <- dettl_config$transform$test
      private$load_ <- load_func
      private$load_test_ <- dettl_config$load$test
      private$test_queries <- dettl_config$load$verification_queries

      private$log_table <- db_get_log_table(db_name, self$path)
      private$confirm <- cfg$db[[db_name]]$confirm
      private$require_branch <- cfg$db[[db_name]]$require_branch
    },

    format = function(brief = FALSE) {
      data_import_format(self, brief, class(self)[[1L]])
    },

    help = function() {
      utils::help(class(self)[[1L]], package = "dettl")
    },

    rollback = function() {
    },

    extract = function() {
      message(sprintf("Running extract %s", self$path))
      private$extracted_data <- run_extract(private$con, private$extract_,
                                            self$path, private$extract_test_)
      invisible(private$extracted_data)
    },

    transform = function() {
      message(sprintf("Running transform %s", self$path))
      private$transformed_data <- run_transform(private$con, private$transform_,
                                                self$path, private$extracted_data,
                                                private$transform_test_)
      invisible(private$transformed_data)
    },

    load = function(comment = NULL, dry_run = FALSE, allow_dirty_git = FALSE) {
      if (!is.null(private$require_branch)) {
        if (git_branch(self$path) != private$require_branch) {
          stop(sprintf("This import can only be run from the '%s' branch",
                       private$require_branch), call. = FALSE)
        }
      }
      if (private$confirm) {
        confirmed <- askYesNo(
          sprintf(
            "About to upload to database '%s' are you sure you want to proceed?",
            private$db_name),
          default = FALSE)
        if (is.na(confirmed) || !confirmed) {
          message("Not uploading to database.")
          return(invisible(FALSE))
        }
      }
      message(sprintf("Running load %s", self$path))
      if (!allow_dirty_git && !dry_run && !git_repo_is_clean(self$path)) {
        stop("Can't run load as repository has unstaged changes. Update git or run in dry-run mode.")
      }
      run_load(private$con, private$load_, private$extracted_data, private$transformed_data,
               private$test_queries, self$path, private$load_test_, dry_run,
               private$log_table, comment)
      invisible(TRUE)
    },

    get_connection = function() {
      private$con
    },

    get_extracted_data = function() {
      private$extracted_data
    },

    get_transformed_data = function() {
      private$transformed_data
    },

    get_log_table = function() {
      private$log_table
    }
  )
)
