extract <- function(con) {
  raw_data <- list()
  raw_data$people <- read.csv("data/people.csv", stringsAsFactors = FALSE)
  raw_data$jobs <- read.csv("data/jobs.csv", stringsAsFactors = FALSE)
  raw_data
}
