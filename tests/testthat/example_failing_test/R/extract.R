extract <- function(con) {
  raw_data <- list()
  raw_data$people <- read.csv(file.path("data", "people.csv"), stringsAsFactors = FALSE)
  raw_data
}
