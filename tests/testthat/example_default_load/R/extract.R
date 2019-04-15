extract <- function(path, con) {
  raw_data <- list()
  raw_data$people <- read.csv(file.path(path, "data/people.csv"), stringsAsFactors = FALSE)
  raw_data$jobs <- read.csv(file.path(path, "data/jobs.csv"), stringsAsFactors = FALSE)
  raw_data
}
