#' Extract data from sources.
#'
#' This step should pull data from sources -  local files or database. And load
#' it into memory ready for transform stage.
#'
#' @param con The active DBI connection for extracting any data.
#'
#' @return A list of data frames representing the extracted data.
#'
#' @keywords internal
extract <- function(con) {
  raw_data <- list()
  raw_data$people <- read.csv("data/people.csv", stringsAsFactors = FALSE)
  raw_data
}
