transform <- function(raw_data) {
  transformed_data <- list()
  transformed_data$hobbies <- raw_data$hobbies
  ## Create a new table
  transformed_data$abode <- data.frame(person = c("Alice", "Bob"),
                                       type = c("house", "flat"),
                                       stringsAsFactors = FALSE)
  transformed_data
}
