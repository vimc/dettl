transform <- function(raw_data) {
  transformed_data <- list()
  transformed_data$people <- raw_data$people[which(raw_data$people$age < 50), ]
  transformed_data
}