extract <- function(path, con) {
  raw_data <- list()
  raw_data$hobbies <- read.csv(file.path(path, "data/hobbies.csv"), stringsAsFactors = FALSE)
  raw_data
}
