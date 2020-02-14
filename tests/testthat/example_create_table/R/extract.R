extract <- function(con) {
  raw_data <- list()
  raw_data$hobbies <- read.csv("data/hobbies.csv", stringsAsFactors = FALSE)
  raw_data
}
