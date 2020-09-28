ImportLog <- R6::R6Class(
  # nolint end
  "ImportLog",
  cloneable = FALSE,

  private = list(
    con = NULL,
    log_data = NULL,
    log_table = NULL,

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
      verify_table(private$con, private$log_table, private$log_data,
                   additional_columns = c("start_time", "end_time", "duration"),
                   context_info = "Cannot import data",
                   solution_text =
                     "Please run dettl::dettl_create_log_table first.")
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
        private$log_data$name)
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

  ),

  public = list(

    initialize = function(con, log_table, path, comment) {
      self$con <- con
      self$log_table <- log_table
      self$log_data <- private$build_log_data(path, comment)
      private$verify_log_table()
      private$verify_first_run()
    },

    start_timer = function() {
      private$log_data$start_time <- private$get_time()
    },

    stop_timer = function() {
      private$log_data$end_time <- private$get_time()
      ## Save the duration in seconds rounded to at most precise the time in ms
      private$log_data$duration <- round(
        as.numeric(private$log_data$end_time) -
          as.numeric(private$log_data$start_time), digits = 3)
    },

    #' @description
    #' Write the log data to the database.
    write_log = function() {
      DBI::dbAppendTable(private$con, private$log_table, private$log_data)
    }
  )
)



