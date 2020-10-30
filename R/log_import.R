#' Generate log data and manage recording timings for an import
#'
#' Class to help help manage initialising log data, managing recording
#' time for log to run and importing row to the log table.
#'
#' @keywords internal
ImportLog <- R6::R6Class(
  # nolint end
  "ImportLog",
  cloneable = FALSE,

  private = list(
    con = NULL,
    log_table = NULL,
    timer_start = NULL,

    get_time = function() {
      time <- Sys.time()
      ## Add tzone attribute so correct time persisted
      attr(time, "tzone") <- "UTC"
      time
    },

    #' @description
    #' Build the data to be written to the import log
    #'
    #' @param import_path Path to the import process directory.
    #' @param comment An optional comment for this import run.
    #'
    #' @return The prepared data for the log table. This is the name
    #' of the import, the date, comment and git information including
    #' user name, user email, current branch and hash of HEAD.
    build_log_data = function(import_path, comment) {
      if (is.null(comment)) {
        comment <- NA_character_
      }
      data_frame(name = basename(import_path),
                 comment = comment,
                 git_user = git_user(import_path),
                 git_email = git_email(import_path),
                 git_branch = git_branch(import_path),
                 git_hash = git_hash(import_path))
    },

    verify_log_table = function() {
      verify_table(private$con, private$log_table, self$log_data,
                   additional_columns = c("start_time", "end_time", "duration"),
                   context_info = "Cannot import data",
                   solution_text =
                     "Please run dettl::dettl_create_log_table first.")
    }

  ),

  public = list(
    #' @field log_data The current log data
    log_data = NULL,

    #' @description
    #' Create import log instance for managing creating log data and
    #' importing to db.
    #'
    #' Initialises log data and verifies that import can be logged. Set
    #' a comment for the log using \code{set_comment}
    #'
    #' @param con Connection to db to add log record to
    #' @param log_table Name of the import log table
    #' @param path Path to import being run
    initialize = function(con, log_table, path) {
      private$con <- con
      private$log_table <- log_table
      self$log_data <- private$build_log_data(path, NULL)
      private$verify_log_table()
      self$verify_first_run()
    },

    #' @description
    #' Set the comment on the log data
    #'
    #' @param comment The comment to be stored with this import.
    set_comment = function(comment) {
      self$log_data$comment <- comment
    },

    #' @description
    #' Start the import log timer
    start_timer = function() {
      private$timer_start <- private$get_time()
      if (is.null(self$log_data$start_time)) {
        self$log_data$start_time <- private$timer_start
      }
    },

    #' @description
    #' Stop the import log timer, adding duration to the total recorded
    #' duration.
    stop_timer = function() {
      self$log_data$end_time <- private$get_time()
      ## Save the duration in seconds rounded to at most precise the time in ms
      this_duration <- round(
        as.numeric(self$log_data$end_time) -
          as.numeric(private$timer_start), digits = 3)
      if (is.null(self$log_data$duration)) {
        self$log_data$duration <- 0
      }
      self$log_data$duration <- self$log_data$duration + this_duration
    },

    #' @description
    #' Write the log data to the database.
    write_log = function() {
      invisible(DBI::dbAppendTable(private$con, private$log_table,
                                   self$log_data))
    },

    #' @description
    #' Verify that this is the first time the import has been run.
    #'
    #' Check whether an import with the same name has been run already. If
    #' so then stop with a human understandable message.
    #'
    #' @return Throws an error if an import with the same name has already been run
    verify_first_run = function() {
      previous_runs <- DBI::dbGetQuery(
        private$con,
        sprintf("SELECT * FROM %s WHERE name = $1", private$log_table),
        self$log_data$name)
      if (nrow(previous_runs) > 0) {
        stop(sprintf("Import has previously been run. Previous run log:
  name:           %s
  start time:     %s
  end time:       %s
  duration:       %s
  comment:        %s
  git user.name:  %s
  git user.email: %s
  git branch:     %s
  git hash:       %s",
                     previous_runs$name,
                     parse_sql_date(private$con, previous_runs$start_time),
                     parse_sql_date(private$con, previous_runs$end_time),
                     previous_runs$duration,
                     previous_runs$comment,
                     previous_runs$git_user,
                     previous_runs$git_email,
                     previous_runs$git_branch,
                     previous_runs$git_hash))
      }
    }
  )
)
